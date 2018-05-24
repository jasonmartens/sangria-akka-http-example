import Serialization._
import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
import akka.http.scaladsl.model.StatusCodes._
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server._
import akka.stream.ActorMaterializer
import sangria.parser.QueryParser
import sangria.schema.Schema
import spray.json.DefaultJsonProtocol._
import spray.json._

import scala.collection.mutable
import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success}

object Server extends App {
  implicit lazy val system: ActorSystem = ActorSystem("sangria-server")
  implicit lazy val materializer: ActorMaterializer = ActorMaterializer()
  implicit lazy val ec: ExecutionContext = system.dispatcher

  lazy val loadedSchemas: mutable.Map[String, Schema[Any, Any]] = mutable.Map.empty

  def loadSchema(id: String, version: String): Schema[Any, Any] = {
    if (loadedSchemas.contains(id)) {
      loadedSchemas(id)
    }
    else {
      val schemaString = Data.lookupByIdAndType("1", "Schema").get("1").asInstanceOf[String]
      val schema = SchemaDefinition.generateSchema(schemaString)
      loadedSchemas(id) = schema
      schema
    }
  }

  lazy val route: Route =
    (post & path("graphql")) {
      // application/graphql
      entity(as[JsObject]) { requestJson ⇒
        val JsObject(fields) = requestJson
        val JsString(query) = fields("query")
        QueryParser.parse(query) match {
          // query parsed successfully, time to execute it!
          case Success(qAst) ⇒
            complete(SchemaDefinition.execute(qAst, loadSchema("1", "1")).map(_.toJson))

          // can't parse GraphQL query, return error
          case Failure(error) ⇒
            complete(BadRequest, JsObject("error" → JsString(error.getMessage)))
        }
      }
      // TODO: Make application/json and application/graphql work - http://graphql.org/learn/serving-over-http/
      // How to use QueryParser with separate variables & operationNames?
    } ~
    get {
      getFromResource("graphiql.html")
    }

  println("Listening on 0.0.0.0:8081")
  Http().bindAndHandle(route, "0.0.0.0", sys.props.get("http.port").fold(8081)(_.toInt))
}
