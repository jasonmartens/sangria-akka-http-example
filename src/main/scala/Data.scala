import scala.collection.mutable

/**
  * Interface for the backend storage system
  */
object Data {
  lazy val data: mutable.Map[(String, String), Map[String, Any]] = mutable.Map(
    ("1000", "Droid") -> Map("id" -> "1000", "name" -> "BB8", "friends" -> Vector("1001", "1002")),
    ("1001", "Droid") -> Map("id" -> "1001", "name" -> "R2D2", "friends" -> Vector("1000")),
    ("1002", "Droid") -> Map("id" -> "1002", "name" -> "C3P0"))

  def lookupByTypeAndId(tpe: String, id: String): Option[Map[String, Any]] = {
    data.get((id, tpe))
  }

  def addObject(id: String, typeName: String, newObject: Map[String, Any]): Unit = {
    data((id, typeName)) = newObject
  }
}