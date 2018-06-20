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
      | interface Character {
      |   id: ID!
      |   name: String!
      | }
      |
      | type Droid implements Character {
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
      |   friends: [ID]
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

  private val extensionSchema =
    """
      | type Jedi implements Character {
      |   id: ID!
      |   name: String!
      |   powers: [String!]!
      | }
      |
      | type Query {
      |   jedi(id: ID!): Jedi
      | }
    """.stripMargin

  // Mock event stream with initial data
  val eventStream: mutable.ListBuffer[Event] = mutable.ListBuffer(
    Create(  Key("A1A47BCE-155E-4567-8926-0F4954AE1E1B", "Droid"),
      Map("id" -> "A1A47BCE-155E-4567-8926-0F4954AE1E1B", "name" -> "BB8")),
    Replace( Key("A1A47BCE-155E-4567-8926-0F4954AE1E1B", "Droid"),
      Map("friends" -> Vector("58BB2C53-F04E-4C02-BA2D-27C3173A833A", "575FE965-9848-462A-B797-373C7AF460AA"))),
    Create(  Key("58BB2C53-F04E-4C02-BA2D-27C3173A833A", "Droid"),
      Map("id" -> "58BB2C53-F04E-4C02-BA2D-27C3173A833A", "name" -> "R2D2",
        "friends" -> Vector("A1A47BCE-155E-4567-8926-0F4954AE1E1B"))),
    Create(  Key("575FE965-9848-462A-B797-373C7AF460AA", "Droid"),
      Map("id" -> "575FE965-9848-462A-B797-373C7AF460AA", "name" -> "C3P0")),
    Create( Key("4B759874-C856-4576-BDE4-FB0EE013CBA4", "Jedi"),
      Map("id" -> "4B759874-C856-4576-BDE4-FB0EE013CBA4", "name" -> "Luke Skywalker")),
    Create(  Key("1", "Schema"),   Map("1" -> defaultSchema)),
    Create(  Key("2", "Schema"),   Map("2" -> extensionSchema))
  )

  def lookupByIdAndType(id: String, tpe: String, schemaSource: String): Option[Map[String, Any]] = {
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

  def addObject(id: String, typeName: String, newObject: Map[String, Any], schemaSource: String): Map[String, Any] = {
    eventStream += Create(Key(id, typeName), newObject)
    newObject + ("id" -> id)
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