import sangria.ast.{Document, FieldDefinition, TypeDefinition}
import sangria.execution.Executor
import sangria.execution.deferred.{Deferred, DeferredResolver}
import sangria.parser.QueryParser
import sangria.schema._
import spray.json._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ExecutionContext, Future}



/**
 * Defines a GraphQL schema for the current project
 */
object SchemaDefinition {
  import sangria.marshalling.sprayJson._

  val sdlSchemaString = """
      | type Droid {
      |  id: ID!
      |  name: String!
      |}
      |
      | type Query {
      |   hero(id: ID!): Droid
      | }
      |
      | schema {
      |   query: Query
      | }
    """.stripMargin
  lazy val ast = QueryParser.parse(sdlSchemaString).get

  case class DeferredString(value: String) extends Deferred[String]
  case class DeferredObject(id: String, fieldName: String, fieldType: sangria.ast.Type) extends Deferred[Map[String, Any]]

  class MyRepo {

    def lookupByTypeAndId(tpe: String, id: String): Option[Map[String, Any]] = {
      (id, tpe) match {
        case ("1000", "hero") => Some(Map("id" -> "1000", "name" -> "BB8"))
        case ("1001", "hero") => Some(Map("id" -> "1001", "name" -> "R2D2"))
        case ("1003", "hero") => Some(Map("id" -> "1002", "name" -> "C3P0"))
        case _ => None
      }
    }

    def loadObjectByIdDeferred[T](tpe: TypeDefinition, field: FieldDefinition, args: Map[String, Any]): DeferredValue[T, Map[String, Any]] = {
      DeferredValue[T, Map[String, Any]](DeferredObject(id = args("id").toString, fieldName = field.name, fieldType = field.fieldType))
    }
  }


  class GenericDeferredResolver extends DeferredResolver[MyRepo] {
    override def resolve(deferred: Vector[Deferred[Any]], ctx: MyRepo, queryState: Any)(implicit ec: ExecutionContext): Vector[Future[Any]] = {
      deferred.map {
        case DeferredString(value) =>
          Future.successful(value)
        case DeferredObject(id, name, tpe) =>
          val data = ctx.lookupByTypeAndId(name, id).get
          Future.successful(data)
      }
    }
  }

  // Builder-based resolvers
  lazy val genericResolver: FieldResolver[MyRepo] = FieldResolver[MyRepo](
    resolve = {
      case (tpe, field) => ctx =>
        if (ctx.args.raw.nonEmpty) {
          ctx.ctx.loadObjectByIdDeferred(tpe, field, ctx.args.raw)
        }
        else {
          ctx.value.asInstanceOf[Map[String, Any]](field.name)
        }
    })

  lazy val builder = AstSchemaBuilder.resolverBased[MyRepo](
    InstanceCheck.field[MyRepo, JsValue],
    genericResolver,
    FieldResolver.defaultInput[MyRepo, JsValue])
  lazy val schema: Schema[MyRepo, Any] = Schema.buildFromAst[MyRepo](ast, builder.validateSchemaWithException(ast))

  def execute(qAst: Document, variables: JsValue) = {
    import sangria.marshalling.sprayJson._
    Executor.execute(
      schema = schema, queryAst = qAst, userContext = new MyRepo, root = JsObject(), variables = variables, deferredResolver = new GenericDeferredResolver)
  }

}
