import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.model.StatusCodes._
import akka.http.scaladsl.server._
import akka.stream.ActorMaterializer
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
import sangria.execution.deferred.DeferredResolver
import sangria.parser.QueryParser
import sangria.execution.{ErrorWithResolver, Executor, QueryAnalysisError}
import sangria.marshalling.sprayJson._
import spray.json._

import scala.util.{Failure, Success}

object Server extends App {
  implicit lazy val system = ActorSystem("sangria-server")
  implicit lazy val materializer = ActorMaterializer()

  import system.dispatcher

  lazy val route: Route =
    (post & path("graphql")) {
      entity(as[JsValue]) { requestJson ⇒
        val JsObject(fields) = requestJson
        val JsString(query) = fields("query")
        QueryParser.parse(query) match {

          // query parsed successfully, time to execute it!
          case Success(qAst) ⇒ complete(SchemaDefinition.execute(qAst))

          // can't parse GraphQL query, return error
          case Failure(error) ⇒
            complete(BadRequest, JsObject("error" → JsString(error.getMessage)))
        }
      }
    } ~
    get {
      getFromResource("graphiql.html")
    }

  println("Listening on 0.0.0.0:8080")
  Http().bindAndHandle(route, "0.0.0.0", sys.props.get("http.port").fold(8080)(_.toInt))
}
