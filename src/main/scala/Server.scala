import Serialization._
import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
import akka.http.scaladsl.model.StatusCodes._
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server._
import akka.stream.ActorMaterializer
import sangria.parser.QueryParser
import spray.json.DefaultJsonProtocol._
import spray.json._

import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success}

object Server extends App {
  implicit lazy val system: ActorSystem = ActorSystem("sangria-server")
  implicit lazy val materializer: ActorMaterializer = ActorMaterializer()
  implicit lazy val ec: ExecutionContext = system.dispatcher

  lazy val route: Route =
    (post & path("graphql")) {
      // application/graphql
      entity(as[JsObject]) { requestJson ⇒
        val JsObject(fields) = requestJson
        val JsString(query) = fields("query")
        QueryParser.parse(query) match {
          // query parsed successfully, time to execute it!
          case Success(qAst) ⇒ complete(SchemaDefinition.execute(qAst).map(_.toJson))

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

  println("Listening on 0.0.0.0:8080")
  Http().bindAndHandle(route, "0.0.0.0", sys.props.get("http.port").fold(8080)(_.toInt))
}
