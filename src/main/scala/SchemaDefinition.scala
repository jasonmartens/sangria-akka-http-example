import sangria.ast.{AstVisitor, ObjectTypeDefinition, Argument => AstArgument, Document => AstDocument, Field => AstField, FieldDefinition => AstFieldDefinition, StringValue => AstStringValue, TypeDefinition => AstTypeDefinition}
import sangria.parser.QueryParser
import sangria.schema._

import scala.collection.mutable

/**
  * Defines a GraphQL schema for the current project
  */
object SchemaDefinition {
  import spray.json._
  implicit object AnyJsonFormat extends JsonFormat[Any] {

    def write(x: Any) = x match {
      case m: Map[String, Any] => JsObject(m.toList.map { case (k, v) => (k, v.toJson) })
      case l: List[Any] => JsArray(l.map(_.toJson).toVector)
      case n: Int => JsNumber(n)
      case s: String => JsString(s)
      case b: Boolean if b == true => JsTrue
      case b: Boolean if b == false => JsFalse
    }

    def read(value: JsValue) = value match {
      case JsNumber(n) => n.intValue()
      case JsString(s) => s
      case JsTrue => true
      case JsFalse => false
    }
  }

  lazy val sdlSchemaString =
    """
      | type Droid {
      |  id: ID!
      |  name: String!
      |  friends: [Droid!]!
      |}
      |
      | type Query {
      |   hero(id: ID!): Droid
      | }
      |
      | schema {
      |   query: Query
      | }
    """.stripMargin
  lazy val ast = QueryParser.parse(sdlSchemaString).get
  lazy val schema = Schema.buildFromAst(ast)

  def lookupByTypeAndId(tpe: String, id: String): Option[Map[String, Any]] = {
    (id, tpe) match {
      case ("1000", "Droid") => Some(Map("id" -> "1000", "name" -> "BB8", "friends" -> Seq("1001", "1002")))
      case ("1001", "Droid") => Some(Map("id" -> "1001", "name" -> "R2D2", "friends" -> Seq("1000")))
      case ("1002", "Droid") => Some(Map("id" -> "1002", "name" -> "C3P0"))
      case _ => None
    }
  }

  def resolveField(astField: AstField, schemaField: Field[_, _], obj: Map[String, Any]): Map[String, Any] = {
    schemaField.fieldType match {
      case ScalarType("ID", _, _, _, _, _, _, _) =>
        Map(astField.name -> obj(astField.name))
      case ScalarType("String", _, _, _, _, _, _, _) =>
        Map(astField.name -> obj(astField.name))
      case ListType(objType@ObjectType(objName, _, _, _, _, _)) =>
        val objectIDs = obj(astField.name).asInstanceOf[List[String]]
        val objects = objectIDs.map { id =>
          resolveObject(id, objType, astField)
        }
        Map(astField.name -> objects)
      case OptionType(objType@ObjectType(objName, _, _, _, _, _)) =>
        astField.arguments.find(_.name == "id") match {
          case Some(AstArgument(_, AstStringValue(value, _, _, _, _), _, _)) =>
            val obj = resolveObject(value, objType, astField)
            Map(astField.name -> obj)
          case None =>
            Map.empty
        }
      case unsupported =>
        ???
    }
  }

  def resolveObject(id: String, objectType: ObjectType[_, _], astField: AstField): Map[String, Any] = {
    val selectedFields: Vector[String] = astField.selections.map { case f: AstField => f.name }
    lookupByTypeAndId(objectType.name, id) match {
      case Some(obj) =>
        obj.filterKeys(k => selectedFields.contains(k)).map { obj => }
        val objFields = astField.selections.map {
          case selectedField: AstField =>
            val schemaField = objectType.fields.find(_.name == selectedField.name).get
            resolveField(selectedField, schemaField, obj)
        }.foldLeft(Map.empty[String, Any])((coll, map) => coll ++ map)
        objFields
      case None => Map.empty
    }
  }

  def execute(qAst: AstDocument): JsValue = {
    import spray.json._
    import DefaultJsonProtocol._

    val queryFieldName = ast.definitions.collect {
      case ObjectTypeDefinition("Query", _, fields, _, _, _, _, _) =>
        fields.head.name
    }.head

    val values = AstVisitor.visitAstWithState(schema, qAst, mutable.Map.empty[String, Any]) { (typeInfo, state) ⇒
      AstVisitor.simple {
        case f: AstField if typeInfo.fieldDef.isDefined && f.name == queryFieldName =>
          val fieldType = typeInfo.fieldDef.get
          state("data") = resolveField(f, fieldType, Map.empty)
      }
    }
    values.toMap.toJson
  }
}
