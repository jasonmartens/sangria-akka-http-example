import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.model.{ContentTypes, HttpEntity, StatusCodes}
import akka.http.scaladsl.testkit.{RouteTestTimeout, ScalatestRouteTest}
import akka.testkit.TestDuration
import org.scalatest.{Matchers, WordSpec}
import sangria.macros._
import spray.json._

import scala.concurrent.duration._
import scala.language.postfixOps


class RouteSpec extends WordSpec with Matchers with ScalatestRouteTest with SprayJsonSupport {
  implicit val timeout = RouteTestTimeout(60.seconds dilated)


  "The service" should {
    "respond to GraphQL queries" in {
      val query = graphql"""{ hero(id: "1000") { id, name } }"""
      val entity = HttpEntity(contentType = ContentTypes.`application/json`, s"""{"query": "${query.renderCompact.replaceAll("\\\"", "\\\\\"")}" } """)

      Post("/graphql", entity) ~> Server.route ~> check {
        response.status shouldBe StatusCodes.OK
        val resp = responseAs[JsValue]
        resp shouldEqual """{"data":{"hero":{"id":"1000","name":"BB8"}}}""".parseJson
      }
    }

    "respond to queries with recursive field types" in {
      val query = graphql"""{ hero(id: "1000") { id name friends {id name} } }"""
      val entity = HttpEntity(contentType = ContentTypes.`application/json`, s"""{"query": "${query.renderCompact.replaceAll("\\\"", "\\\\\"")}" } """)

      Post("/graphql", entity) ~> Server.route ~> check {
        response.status shouldBe StatusCodes.OK
        val resp = responseAs[JsValue]
        resp shouldEqual """{"data":{"hero":{"id":"1000","name":"BB8","friends":[{"id":"1001","name":"R2D2"},{"id":"1002","name":"C3P0"}]}}}""".parseJson
      }
    }

    "create new objects" in {
      val query = graphql"""mutation { createDroid(droid: {name: "BB9" friends: ["1000"] } ) { id name friends {id name} } }"""
      val entity = HttpEntity(contentType = ContentTypes.`application/json`, s"""{"query": "${query.renderCompact.replaceAll("\\\"", "\\\\\"")}" } """)

      Post("/graphql", entity) ~> Server.route ~> check {
        response.status shouldBe StatusCodes.OK
        val resp = responseAs[JsValue]
        val JsString(newId) = resp.asJsObject.getFields("data").head.asJsObject.getFields("createDroid").head.asJsObject.getFields("id").head
        resp shouldEqual s"""{"data":{"createDroid":{"id":"$newId","name":"BB9","friends":[{"id":"1000","name":"BB8"}]}}}""".parseJson
      }
    }
  }
}
