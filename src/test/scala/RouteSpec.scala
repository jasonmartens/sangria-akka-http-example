import akka.http.scaladsl.model.{ContentTypes, HttpEntity, StatusCodes}
import akka.http.scaladsl.testkit.{RouteTestTimeout, ScalatestRouteTest}
import akka.testkit.TestDuration
import de.heikoseeberger.akkahttpcirce.ErrorAccumulatingCirceSupport._
import io.circe._
import org.scalatest.{Matchers, WordSpec}
import sangria.macros._

import scala.concurrent.duration._
import scala.language.postfixOps


class RouteSpec extends WordSpec with Matchers with ScalatestRouteTest {
  implicit val timeout = RouteTestTimeout(60.seconds dilated)

  "The service" should {
    "respond to GraphQL queries" in {
      val query = graphql"""{ hero(id: "A1A47BCE-155E-4567-8926-0F4954AE1E1B") { id, name } }"""
      val entity = HttpEntity(contentType = ContentTypes.`application/json`, s"""{"query": "${query.renderCompact.replaceAll("\\\"", "\\\\\"")}" } """)

      Post("/graphql", entity) ~> Server.route ~> check {
        response.status shouldBe StatusCodes.OK
        val resp = responseAs[Json]
        resp.noSpaces shouldEqual """{"data":{"hero":{"id":"A1A47BCE-155E-4567-8926-0F4954AE1E1B","name":"BB8"}}}"""
      }
    }

    "respond to queries with recursive field types" in {
      val query = graphql"""{ hero(id: "A1A47BCE-155E-4567-8926-0F4954AE1E1B") { id name friends {id name} } }"""
      val entity = HttpEntity(contentType = ContentTypes.`application/json`, s"""{"query": "${query.renderCompact.replaceAll("\\\"", "\\\\\"")}" } """)

      Post("/graphql", entity) ~> Server.route ~> check {
        response.status shouldBe StatusCodes.OK
        val resp = responseAs[Json]
        resp.noSpaces shouldEqual """{"data":{"hero":{"id":"A1A47BCE-155E-4567-8926-0F4954AE1E1B","name":"BB8","friends":[{"id":"58BB2C53-F04E-4C02-BA2D-27C3173A833A","name":"R2D2"},{"id":"575FE965-9848-462A-B797-373C7AF460AA","name":"C3P0"}]}}}"""
      }
    }

    "create new objects" in {
      val query = graphql"""mutation { createDroid(droid: {name: "BB9" friends: ["A1A47BCE-155E-4567-8926-0F4954AE1E1B"] } ) { id name friends {id name} } }"""
      val entity = HttpEntity(contentType = ContentTypes.`application/json`, s"""{"query": "${query.renderCompact.replaceAll("\\\"", "\\\\\"")}" } """)

      Post("/graphql", entity) ~> Server.route ~> check {
        response.status shouldBe StatusCodes.OK
        val resp = responseAs[Json]
        val newId = resp.hcursor.downField("data").downField("createDroid").get[String]("id").toOption.get
        resp.noSpaces shouldEqual s"""{"data":{"createDroid":{"id":"$newId","name":"BB9","friends":[{"id":"A1A47BCE-155E-4567-8926-0F4954AE1E1B","name":"BB8"}]}}}"""
      }
    }
  }
}
