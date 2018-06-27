import java.util.UUID

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.StatusCodes._
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server._
import akka.stream.ActorMaterializer
import de.heikoseeberger.akkahttpcirce.ErrorAccumulatingCirceSupport._
import io.circe._
import io.circe.optics.JsonPath._
import io.circe.parser._
import sangria.ast.OperationType
import sangria.ast.{Document, Type => ASTType, _}
import sangria.execution.deferred.{Deferred, DeferredResolver}
import sangria.execution.{ErrorWithResolver, Executor, MaterializedSchemaValidationError, QueryAnalysisError}
import sangria.marshalling.circe._
import sangria.parser.DeliveryScheme.Try
import sangria.parser.{AggregateSourceMapper, ParserConfig, QueryParser, SyntaxError}
import sangria.schema.AstSchemaBuilder.TypeName
import sangria.schema.{Action, AstSchemaBuilder, Context, FieldResolver, Schema}

import scala.collection.immutable.Map
import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal
import scala.util.{Failure, Success}


object Server extends App {
  implicit lazy val system: ActorSystem = ActorSystem("sangria-server")
  implicit lazy val materializer: ActorMaterializer = ActorMaterializer()
  implicit lazy val ec: ExecutionContext = system.dispatcher

  def executeGraphQL(query: Document, operationName: Option[String], variables: Json) =
    complete(Executor.execute(buildSchema, query,
      variables = if (variables.isNull) Json.obj() else variables,
      deferredResolver = new MyDeferredResolver,
      operationName = operationName)
      .map(OK → _)
      .recover {
        case error: QueryAnalysisError ⇒ BadRequest → error.resolveError
        case error: ErrorWithResolver ⇒ InternalServerError → error.resolveError
        case error: MaterializedSchemaValidationError => InternalServerError -> error.resolveError
      })

  def formatError(error: Throwable): Json = error match {
    case syntaxError: SyntaxError ⇒
      Json.obj("errors" → Json.arr(
        Json.obj(
          "message" → Json.fromString(syntaxError.getMessage),
          "locations" → Json.arr(Json.obj(
            "line" → Json.fromBigInt(syntaxError.originalError.position.line),
            "column" → Json.fromBigInt(syntaxError.originalError.position.column))))))
    case NonFatal(e) ⇒
      formatError(e.getMessage)
  }

  def formatError(message: String): Json =
    Json.obj("errors" → Json.arr(Json.obj("message" → Json.fromString(message))))

  lazy val myResolver: FieldResolver[Any] = FieldResolver {
    case (TypeName(name), field) ⇒ ctx => recursiveResolver(field, ctx)
  }

  /**
    * Need to merge any definitions with a name of (Query|Mutation|Subscription),
    * as well as any types defined as a (Query|Mutation|Subscription) in the schema {} section
    * @return
    */
  def stitchOperation(definitions: Vector[Definition], stitchOpType: OperationType): Vector[Definition] = {
    val stitchOpName = stitchOpType match {
      case OperationType.Query => "Query"
      case OperationType.Mutation => "Mutation"
      case OperationType.Subscription => "Subscription"
    }

    // Fields from all definitions with a name of Query
    val queryFields: Vector[FieldDefinition] = definitions.collect {
      case ObjectTypeDefinition(opName, _, fields, _, _, _, _, _) if opName == stitchOpName => fields }.flatten

    // All type definitions from the SchemaDefinition with an OperationTypeDefinition of stitchedOptType
    val schemaObjectTypeNames = definitions
      .collect { case SchemaDefinition(opTypes, _, _, _, _) => opTypes }
      .flatten.collect { case OperationTypeDefinition(op, tpe, _, _) if op == stitchOpType => tpe.name }
    // From all the NamedTypes in the SchemaDefinition for queries, get the fields
    val schemaQueryTypesFields: Vector[FieldDefinition] = definitions
      .collect {
        case ObjectTypeDefinition(name, _, fields, _, _, _, _, _) if schemaObjectTypeNames.contains(name) => fields }
      .flatten

    val stitchedOperationObject = ObjectTypeDefinition(s"Stitched$stitchOpName", Vector.empty, schemaQueryTypesFields ++ queryFields)
    val queryOpTypeDef = OperationTypeDefinition(stitchOpType, NamedType(s"Stitched$stitchOpName"))

    // If there is nothing to stitch, then there are no operations of this type in the schema.
    // Return the existing Schema Definition, or create an empty one.
    val schemaDefinition: SchemaDefinition = if (stitchedOperationObject.fields.isEmpty) {
      definitions.collectFirst[SchemaDefinition] {
        case sd @ SchemaDefinition(_, _, _, _, _) => sd
      }.getOrElse (SchemaDefinition(Vector.empty))
    } else {
      // Replace any existing SchemaDefinitions with this stitchOpType with a new combined definition
      definitions.collectFirst[SchemaDefinition] {
        case sd @ SchemaDefinition(_, _, _, _, _) =>
          val unmodifiedOperationTypes: Vector[OperationTypeDefinition] = sd.operationTypes
            .collect{ case otd @ OperationTypeDefinition(op, _, _, _) if op != stitchOpType => otd}
          sd.copy(operationTypes = unmodifiedOperationTypes :+ queryOpTypeDef)}
        // If there is no existing SchemaDefinition
        // (because schemas used named Query|Mutation|Subscription objects), create a new one
        .getOrElse{
        SchemaDefinition(operationTypes = Vector(
          OperationTypeDefinition(stitchOpType, NamedType(s"Stitched$stitchOpName", None), Vector.empty, None) ))}
    }

    // Filter out stitched items
    val unmodifiedDefinitions: Vector[Definition] = definitions.filter {
      case ObjectTypeDefinition(opName, _, _, _, _, _, _, _) if opName == stitchOpName => false
        // Remove objects that were combined into the StitchedObject
      case ObjectTypeDefinition(opName, _, _, _, _, _, _, _) if schemaObjectTypeNames.contains(opName) => false
      case SchemaDefinition(_, _, _, _, _) => false
      case _ => true
    }

    if (stitchedOperationObject.fields.isEmpty) {
      unmodifiedDefinitions :+ schemaDefinition
    }
    else {
      unmodifiedDefinitions :+ stitchedOperationObject :+ schemaDefinition
    }
  }

  def stitchSchemas(documents: Traversable[Document]): Document = {
    val originalSourceMappers = documents.flatMap(_.sourceMapper).toVector
    val sourceMapper =
      if (originalSourceMappers.nonEmpty) Some(AggregateSourceMapper.merge(originalSourceMappers))
      else None

    val definitions: Vector[Definition] = documents.flatMap(_.definitions).toVector
    val stitchedQueries = stitchOperation(definitions, OperationType.Query)
    val stitchedMutations = stitchOperation(stitchedQueries, OperationType.Mutation)
    val stitchedSubscriptions = stitchOperation(stitchedMutations, OperationType.Subscription)

    Document(stitchedSubscriptions, Vector.empty, None, sourceMapper)
  }

  def buildSchema = {
    val schemaString = Data.lookupByIdAndType("1", "Schema", "").get("1").asInstanceOf[String]
    val extensionSchemaString = Data.lookupByIdAndType("2", "Schema", "").get("2").asInstanceOf[String]
    val builder = AstSchemaBuilder.resolverBased(myResolver)
    val schemaDoc = QueryParser.parse(schemaString, config = ParserConfig(sourceIdFn = _ => "1")).get
    val extensionDoc = QueryParser.parse(extensionSchemaString, config = ParserConfig(sourceIdFn = _ => "2")).get
    val mergedDoc = stitchSchemas(Vector(schemaDoc, extensionDoc))
    Schema.buildFromAst(mergedDoc, builder.validateSchemaWithException(mergedDoc))
  }

  class MyDeferredResolver extends DeferredResolver[Any] {
    def resolve(deferred: Vector[Deferred[Any]], ctx: Any, queryState: Any)(implicit ec: ExecutionContext) =
      deferred.map { d =>
        Future.successful(d.asInstanceOf[DeferredMap].items.map(item => Data.lookupByIdAndType(item.id, item.tpe, item.location)))
      }
  }

  case class DeferredItem(id: String, tpe: String, location: String)
  case class DeferredMap(items: Seq[DeferredItem]) extends Deferred[Seq[Map[String, Any]]]

  def recursiveResolver[Ctx](field: FieldDefinition, ctx: Context[Any, _]): Action[Ctx, Any] = {
    def fieldTypeResolver(ftype: ASTType): Action[Ctx, Any] = {
      ftype match {
        case NamedType("String", _) =>
          ctx.value match {
            case Some(map) =>
              map.asInstanceOf[Map[String, Any]](field.name)
            case map =>
              map.asInstanceOf[Map[String, Any]](field.name)
          }
        case NamedType("ID", _) =>
          ctx.value match {
            case Some(map) =>
              map.asInstanceOf[Map[String, Any]](field.name)
            case map =>
              map.asInstanceOf[Map[String, Any]](field.name)
          }
        case NamedType(tpeName, loc) =>
          ctx.query.operations.head._2.operationType match {
            case OperationType.Mutation =>
              Data.addObject(UUID.randomUUID().toString, tpeName, ctx.args.arg(field.arguments.head.name), loc.get.sourceId)
            case OperationType.Query =>
              Data.lookupByIdAndType(ctx.args.arg(field.arguments.head.name).toString, tpeName, loc.get.sourceId)
          }
        case NotNullType(subType, _) =>
          fieldTypeResolver(subType)
        case ListType(subType, loc) =>
          // TODO: How can I avoid writing additional nested match statements here?
          subType match {
            case NotNullType(NamedType("String", _), _) =>
              ctx.value.asInstanceOf[Map[String, Any]](field.name).asInstanceOf[Vector[String]]
            case NotNullType(NamedType(tpeName, namedLocation), nullLocation) =>
              val listIDs = ctx.value.asInstanceOf[Map[String, Any]](field.name).asInstanceOf[Vector[String]]
                .map(id => DeferredItem(id, tpeName, namedLocation.get.sourceId))
              DeferredMap(listIDs)
            case other =>
              println(s"Got other type: $other")
              ???
          }
        case bork =>
          println(s"got bork: $bork")
      }
    }
    fieldTypeResolver(field.fieldType)
  }

  lazy val route: Route =
    (post & path("graphql")) {
      parameters('query.?, 'operationName.?, 'variables.?) { (queryParam, operationNameParam, variablesParam) ⇒
          entity(as[Json]) { body ⇒
          val query = queryParam orElse root.query.string.getOption(body)
          val operationName = operationNameParam orElse root.operationName.string.getOption(body)
          val variablesStr = variablesParam orElse root.variables.string.getOption(body)
          query.map(QueryParser.parse(_)) match {
            case Some(Success(ast)) ⇒
              variablesStr.map(parse) match {
                case Some(Left(error)) ⇒ complete(BadRequest, formatError(error))
                case Some(Right(json)) ⇒
                  executeGraphQL(ast, operationName, json)
                case None ⇒
                  executeGraphQL(ast, operationName, root.variables.json.getOption(body) getOrElse Json.obj())
              }
            case Some(Failure(error)) ⇒ complete(BadRequest, formatError(error))
            case None ⇒ complete(BadRequest, formatError("No query to execute"))
          }
        }
      }
    } ~
      get {
        getFromResource("graphiql.html")
      }

  println("Listening on 0.0.0.0:8081")
  Http().bindAndHandle(route, "0.0.0.0", sys.props.get("http.port").fold(8081)(_.toInt))
}
