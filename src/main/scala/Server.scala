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
import sangria.execution.{ErrorWithResolver, Executor, QueryAnalysisError}
import sangria.marshalling.circe._
import sangria.parser.DeliveryScheme.Try
import sangria.parser.{QueryParser, SyntaxError}
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
    case e ⇒
      throw e
  }

  def formatError(message: String): Json =
    Json.obj("errors" → Json.arr(Json.obj("message" → Json.fromString(message))))

  lazy val myResolver: FieldResolver[Any] = FieldResolver {
    case (TypeName(name), field) ⇒ ctx => recursiveResolver(field, ctx)
  }

  def buildSchema = {
    val schemaString = Data.lookupByIdAndType("1", "Schema").get("1").asInstanceOf[String]
    val builder = AstSchemaBuilder.resolverBased(myResolver)
    val schemaDoc = QueryParser.parse(schemaString).get
    Schema.buildFromAst(schemaDoc, builder.validateSchemaWithException(schemaDoc))
  }

  class MyDeferredResolver extends DeferredResolver[Any] {
    def resolve(deferred: Vector[Deferred[Any]], ctx: Any, queryState: Any)(implicit ec: ExecutionContext) =
      deferred.map { d =>
        Future.successful(d.asInstanceOf[DeferredMap].items.map(Data.lookupByIdAndType(_, "Droid")))
      }
  }

  case class DeferredMap(items: Seq[String]) extends Deferred[Seq[Map[String, Any]]]

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
              Data.addObject(UUID.randomUUID().toString, tpeName, ctx.args.arg(field.arguments.head.name))
            case OperationType.Query =>
              Data.lookupByIdAndType(ctx.args.arg(field.arguments.head.name).toString, tpeName)
          }
        case NotNullType(subType, _) =>
          fieldTypeResolver(subType)
        case ListType(subType, _) =>
          val listIDs = ctx.value.asInstanceOf[Map[String, Any]](field.name).asInstanceOf[Seq[String]]
          DeferredMap(listIDs)
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
