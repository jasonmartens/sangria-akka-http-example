import akka.actor.ActorSystem
import akka.http.scaladsl.model.{HttpCharsets, MediaType}
import akka.http.scaladsl.model.MediaType.WithFixedCharset
import akka.http.scaladsl.settings.{ParserSettings, ServerSettings}

class CustomMediaTypes(system: ActorSystem) {
  // Add GraphQL media type
  private val utf8 = HttpCharsets.`UTF-8`
  val `application/graphql`: WithFixedCharset = MediaType.customWithFixedCharset("application", "custom", utf8)

  val parserSettings: ParserSettings = ParserSettings(system).withCustomMediaTypes(`application/graphql`)
  val serverSettings: ServerSettings = ServerSettings(system).withParserSettings(parserSettings)
}
