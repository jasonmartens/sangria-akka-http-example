import java.util.UUID

import sangria.ast.{Document, FieldDefinition, ObjectTypeDefinition, TypeDefinition}
import sangria.execution.{ExecutionScheme, Executor}
import sangria.execution.deferred.{Deferred, DeferredResolver, Fetcher, HasId}
import sangria.parser.QueryParser
import sangria.schema._
import spray.json._
import sangria.macros._
import sangria.marshalling.{InputUnmarshaller, ResultMarshaller}
import sangria.marshalling.sprayJson._

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.ExecutionContext.Implicits.global



/**
 * Defines a GraphQL schema for the current project
 */
object SchemaDefinition {
  import sangria.marshalling.sprayJson._
  import DefaultJsonProtocol._

  val sdlSchemaString = """
      | type Human {
      |  id: ID!
      |  name: String!
      |}
      |
      | type Query {
      |   hero(id: ID!): Human
      | }
      |
      | schema {
      |   query: Query
      | }
    """.stripMargin
  lazy val ast = QueryParser.parse(sdlSchemaString).get

  class MyRepo {
    def loadObject(tpe: TypeDefinition, field: FieldDefinition) : String = {
        if (tpe.name == "Human" && field.name == "id")
          "1"
        else if (tpe.name == "Human" && field.name == "name")
          "BB8"
        else {
          ""
      }
    }

    def loadObjects(ids: Seq[UUID]): Future[Seq[JsObject]] = {
      Future.successful(Seq(JsObject()))
    }
  }

  // Fetcher/Deferred resolvers
//  implicit val hasId = HasId[JsObject, UUID](_.fields.get("id").map(id => UUID.fromString(id.convertTo[String])).get)
//  lazy val genericFetcher = Fetcher((ctx: MyRepo, ids: Seq[UUID]) ⇒ ctx.loadObjects(ids))
//  lazy val fetcherResolver: DeferredResolver[MyRepo] = DeferredResolver.fetchers(genericFetcher)

//  lazy val fetcherGenericResolver: FieldResolver[MyRepo] = FieldResolver[MyRepo](
//    resolve = {
//      case (tpe, field) => ctx =>
//        genericFetcher.defer(UUID.randomUUID())
//    }
//  )


  class GenericDeferredResolver extends DeferredResolver[Any] {
    def resolve(deferred: Vector[Deferred[Any]], ctx: Any, queryState: Any)(implicit ec: ExecutionContext): Vector[Future[JsObject]] = {
        Vector(Future.successful(JsObject(("hello", JsString("world")))))
    }
  }
//  lazy val fetcherResolver: sangria.execution.deferred.DeferredResolver[Any] = new GenericDeferredResolver

  // Builder-based resolvers
  lazy val genericResolver: FieldResolver[MyRepo] = FieldResolver[MyRepo](
    resolve = {
      case (tpe, field) => ctx =>
        Value(ctx.ctx.loadObject(tpe, field))
    })
  lazy val builder = AstSchemaBuilder.resolverBased[MyRepo](
    InstanceCheck.field[MyRepo, JsValue],
    genericResolver,
    FieldResolver.defaultInput[MyRepo, JsValue])
  lazy val schema: Schema[MyRepo, Any] = Schema.buildFromAst[MyRepo](ast, builder.validateSchemaWithException(ast))

//  lazy val queryAst: Document = QueryParser.parse("""{ hero { id name } }""").get

  lazy val nonBuilderSchema: Schema[Any, Any] = Schema.buildFromAst(ast)

  def execute(qAst: Document, variables: JsValue) = {
    import sangria.marshalling.sprayJson._
    Executor.execute(
      schema = schema, queryAst = qAst, userContext = new MyRepo, root = JsObject(), variables = variables, deferredResolver = new GenericDeferredResolver)
  }

}
