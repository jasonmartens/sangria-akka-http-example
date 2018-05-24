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

  private val defaultSchema = """
      |
      | type Droid {
      |  "The ID of the droid"
      |  id: ID!
      |  "How we refer to the droid in the movies"
      |  name: String!
      |  friends: [Droid!]!
      |}
      |
      | input NewDroid {
      |   name: String!
      |   friends: [ID!]!
      | }
      |
      | input UpdateDroid {
      |   id: ID!
      |   name: String
      |   @append friends: [ID]
      | }
      |
      | type Query {
      |   hero(id: ID!): Droid
      | }
      |
      | type Mutation {
      |   createDroid(droid: NewDroid!): Droid
      |   modifyDroid(droid: UpdateDroid!): Droid
      | }
      |
      | schema {
      |   query: Query
      |   mutation: Mutation
      | }
    """.stripMargin

  // Mock event stream with initial data
  val eventStream: mutable.ListBuffer[Event] = mutable.ListBuffer(
    Create(  Key("1000", "Droid"), Map("id" -> "1000", "name" -> "BB8")),
    Replace( Key("1000", "Droid"), Map("friends" -> Vector("1001", "1002"))),
    Create(  Key("1001", "Droid"), Map("id" -> "1001", "name" -> "R2D2", "friends" -> Vector("1000"))),
    Create(  Key("1002", "Droid"), Map("id" -> "1002", "name" -> "C3P0")),
    Create(  Key("1", "Schema"),   Map("1" -> defaultSchema))
  )

  def lookupByIdAndType(id: String, tpe: String): Option[Map[String, Any]] = {
    val lookupKey = Key(id, tpe)
    val d = collapseEvents(eventStream.filter(e => e.key == lookupKey).toList)
    if (d.isEmpty) None else Some(d(lookupKey))
  }

  def collapseEvents(events: List[Event]): Map[Key, Map[String, Any]] = {
    events.groupBy(_.key)
      .map{case (key, keyEvents) =>
        val keyMap: Map[String, Any] = keyEvents.foldLeft(Map.empty[String, Any]){
          case (coll, Create(_, d)) => coll ++ d
          case (coll, Replace(_, d)) => coll ++ d}
        (key, keyMap)}
  }

  def addObject(id: String, typeName: String, newObject: Map[String, Any]): Unit = {
    eventStream += Create(Key(id, typeName), newObject)
  }

  /**
    * For a given referenceFieldName, find all the entries containing id and return a list of the object keys
    * @param id The referenced ID to lookup
    * @param referenceFieldName The name of the field containing references. Must be a Vector[String]
    * @return A Vector of Keys where referenceFieldName contains ID
    */
  def lookupReferences(id: String, referenceFieldName: String): Vector[Key] = {
    collapseEvents(eventStream.toList).collect{ case (key, data) =>
      data.get(referenceFieldName)
        .map(x => x.asInstanceOf[Vector[String]].contains(id)) match {
          case Some(true) => key
          case _ => ???
        }
    }.toVector
  }
}