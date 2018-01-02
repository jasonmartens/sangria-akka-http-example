import scala.collection.mutable

/**
  * Interface for the backend storage system
  */
object Data {
  case class Key(id: String, objType: String)

  sealed trait Event {
    val key: Key
  }
  case class Create(key: Key, data: Map[String, Any]) extends Event
  case class Replace(key: Key, data: Map[String, Any]) extends Event

  // Mock event stream with initial data
  val eventStream: mutable.ListBuffer[Event] = mutable.ListBuffer(
    Create(  Key("1000", "Droid"), Map("id" -> "1000", "name" -> "BB8")),
    Replace( Key("1000", "Droid"), Map("friends" -> Vector("1001", "1002"))),
    Create(  Key("1001", "Droid"), Map("id" -> "1001", "name" -> "R2D2", "friends" -> Vector("1000"))),
    Create(  Key("1002", "Droid"), Map("id" -> "1002", "name" -> "C3P0"))
  )

  def lookupByTypeAndId(tpe: String, id: String): Option[Map[String, Any]] = {
    val lookupKey = Key(id, tpe)
    val d = eventStream.filter(e => e.key == lookupKey).foldLeft(Map.empty[String, Any]){
      case (coll, Create(_, d)) => coll ++ d
      case (coll, Replace(_, d)) => coll ++ d
    }
    if (d.isEmpty) None else Some(d)
  }

  def addObject(id: String, typeName: String, newObject: Map[String, Any]): Unit = {
    eventStream += Create(Key(id, typeName), newObject)
  }
}