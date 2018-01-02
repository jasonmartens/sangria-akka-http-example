import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.model.StatusCodes._
import akka.http.scaladsl.server._
import akka.stream.ActorMaterializer
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
import sangria.parser.QueryParser
import spray.json._
import DefaultJsonProtocol._
import Serialization._
import akka.http.scaladsl.model.MediaType.WithFixedCharset
import akka.http.scaladsl.model.{ContentType, HttpCharsets, MediaType, MediaTypes}
import sangria.execution.Executor

import scala.util.{Failure, Success}

object Server extends App {
  implicit lazy val system = ActorSystem("sangria-server")
  implicit lazy val materializer = ActorMaterializer()
  lazy val customMediaTypes = new CustomMediaTypes(system)
  import customMediaTypes._


  lazy val route: Route =
    (post & path("graphql") & extract(_.request)) { request =>
      request.entity.contentType.mediaType match {
        case MediaTypes.`application/json` =>
          entity(as[JsValue]) { requestJson ⇒
            // TODO: Support all the recommended endpoints at http://graphql.org/learn/serving-over-http/
            val JsObject(fields) = requestJson
            val JsString(query) = fields("query")
            val operation = fields.get("operationName") collect {
              case JsString(op) ⇒ op
            }
            val vars = fields.get("variables") match {
              case Some(obj: JsObject) ⇒ obj
              case _ ⇒ JsObject.empty
            }

            // TODO: How to parse a query with separate vars and operation?
            QueryParser.parse(query) match {
              // query parsed successfully, time to execute it!
              case Success(qAst) ⇒ complete(SchemaDefinition.execute(qAst).toJson)

              // can't parse GraphQL query, return error
              case Failure(error) ⇒
                complete(BadRequest, JsObject("error" → JsString(error.getMessage)))
            }
          }
        case `application/graphql` =>
          entity(as[String]) { queryBody ⇒
            // application/graphql
            QueryParser.parse(queryBody) match {
              // query parsed successfully, time to execute it!
              case Success(qAst) ⇒ complete(SchemaDefinition.execute(qAst).toJson)

              // can't parse GraphQL query, return error
              case Failure(error) ⇒
                complete(BadRequest, JsObject("error" → JsString(error.getMessage)))
            }
          }
      }
    } ~
    get {
      getFromResource("graphiql.html")
    }

  println("Listening on 0.0.0.0:8080")
  Http().bindAndHandle(route, "0.0.0.0", sys.props.get("http.port").fold(8080)(_.toInt), settings = customMediaTypes.serverSettings)
}
