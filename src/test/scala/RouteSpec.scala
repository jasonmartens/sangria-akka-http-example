import org.scalatest.{Matchers, WordSpec}
import akka.http.scaladsl.model.{ContentTypes, HttpEntity, StatusCodes}
import akka.http.scaladsl.testkit.ScalatestRouteTest
import akka.http.scaladsl.server._
import Directives._
import spray.json._
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport

import sangria.macros._
import scala.concurrent.duration._
import akka.http.scaladsl.testkit.RouteTestTimeout
import akka.testkit.TestDuration

import scala.language.postfixOps


class RouteSpec extends WordSpec with Matchers with ScalatestRouteTest with SprayJsonSupport {
  implicit val timeout = RouteTestTimeout(60.seconds dilated)
  import DefaultJsonProtocol._


  "The service" should {
    //    "Return graphiql" in {
    //      Get("/") ~> Server.route ~> check {
    //        responseAs[String] shouldEqual ""
    //      }
    //    }

    "respond to GraphQL queries" in {
      val query = graphql"""{ hero(id: "1000") { id, name } }"""
      val entity = HttpEntity(contentType = ContentTypes.`application/json`, s"""{"query": "${query.renderCompact.replaceAll("\\\"", "\\\\\"")}" } """)

      Post("/graphql", entity) ~> Server.route ~> check {
//        response.status shouldBe StatusCodes.OK
        val resp = responseAs[JsValue]
        resp shouldEqual """{"data":{"hero":{"id":"1","name":"BB8"}}}""".parseJson
      }
    }
  }
}
