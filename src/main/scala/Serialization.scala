
/**
  * Methods to serialize/deserialize data
  */
object Serialization {
  import spray.json._
  implicit object AnyJsonFormat extends JsonFormat[Any] {

    def write(x: Any) = x match {
      case m: Map[String, Any] => JsObject(m.toList.map { case (k, v) => (k, v.toJson) })
      case l: List[Any] => JsArray(l.map(_.toJson).toVector)
      case v: Vector[Any] => JsArray(v.map(_.toJson))
      case n: Int => JsNumber(n)
      case s: String => JsString(s)
      case b: Boolean if b == true => JsTrue
      case b: Boolean if b == false => JsFalse
      case o: Some[Any] => o.get.toJson
      case None => JsNull
      case null => JsNull
    }

    def read(value: JsValue) = value match {
      case JsNumber(n) => n.intValue()
      case JsString(s) => s
      case JsTrue => true
      case JsFalse => false
    }
  }
}
