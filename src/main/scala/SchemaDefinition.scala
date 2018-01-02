import java.util.UUID

import sangria.ast.{AstVisitor, ObjectTypeDefinition, Argument => AstArgument, Document => AstDocument, Field => AstField, FieldDefinition => AstFieldDefinition, OperationDefinition => AstOperationDefinition, OperationType => AstOperationType, Selection => AstSelection, StringValue => AstStringValue, TypeDefinition => AstTypeDefinition, Value => AstValue, ObjectValue => AstObjectValue, ObjectField => AstObjectField, ListValue => AstListValue, NameValue => AstNameValue, ScalarValue => AstScalarValue}
import sangria.parser.QueryParser
import sangria.schema._
import sangria.validation.QueryValidator

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
      case v: Vector[Any] => JsArray(v.map(_.toJson))
      case n: Int => JsNumber(n)
      case s: String => JsString(s)
      case b: Boolean if b == true => JsTrue
      case b: Boolean if b == false => JsFalse
      case o: Some[Any] => o.get.toJson
      case None => JsNull
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
      |
      | type Droid {
      |  id: ID!
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
  lazy val ast = QueryParser.parse(sdlSchemaString).get
  lazy val schema = Schema.buildFromAst(ast)
  lazy val data: mutable.Map[(String, String), Map[String, Any]] = mutable.Map(
    ("1000", "Droid") -> Map("id" -> "1000", "name" -> "BB8", "friends" -> Vector("1001", "1002")),
    ("1001", "Droid") -> Map("id" -> "1001", "name" -> "R2D2", "friends" -> Vector("1000")),
    ("1002", "Droid") -> Map("id" -> "1002", "name" -> "C3P0"))

  def lookupByTypeAndId(tpe: String, id: String): Option[Map[String, Any]] = {
    data.get((id, tpe))
  }

  def resolveField(astField: AstField, outputType: OutputType[_], obj: Map[String, Any]): Map[String, Any] = {
    outputType match {
      case ScalarType("ID", _, _, _, _, _, _, _) =>
        Map(astField.name -> obj(astField.name))
      case ScalarType("String", _, _, _, _, _, _, _) =>
        Map(astField.name -> obj(astField.name))
      case ListType(objType@ObjectType(objName, _, _, _, _, _)) =>
        val objectIDs = obj(astField.name).asInstanceOf[Vector[String]]
        val objects = objectIDs.map { id =>
          resolveObject(id, objType, astField.selections)
        }
        Map(astField.name -> objects)
      case OptionType(objType@ObjectType(objName, _, _, _, _, _)) =>
        astField.arguments.find(_.name == "id") match {
          case Some(AstArgument(_, AstStringValue(value, _, _), _, _)) =>
            val obj = resolveObject(value, objType, astField.selections)
            Map(astField.name -> obj)
          case None =>
            Map.empty
        }
      case unsupported =>
        ???
    }
  }

  def resolveObject(id: String, objectType: ObjectType[_, _], selections: Vector[AstSelection]): Map[String, Any] = {
    val selectedFields: Vector[String] = selections.map { case f: AstField => f.name }
    lookupByTypeAndId(objectType.name, id) match {
      case Some(obj) =>
        obj.filterKeys(k => selectedFields.contains(k)).map { obj => }
        val objFields = selections.map {
          case selectedField: AstField =>
            val schemaField = objectType.fields.find(_.name == selectedField.name).get
            resolveField(selectedField, schemaField.fieldType, obj)
        }.foldLeft(Map.empty[String, Any])((coll, map) => coll ++ map)
        objFields
      case None => Map.empty
    }
  }

  def resolveInputField(value: AstValue): Any = {
    value match {
      case AstObjectValue(fields, _, _) =>
        fields.foldLeft(Map.empty[String, Any])((coll, f) => coll ++ Map(f.name -> resolveInputField(f.value)))
      case objField: AstObjectField =>
        Map(objField.name -> resolveInputField(objField.value))
      case AstStringValue(v, _, _) =>
        v
      case AstListValue(vs, _, _) =>
        vs.map(resolveInputField)
      case _ =>
        ???
    }
  }

  def createObject(objValue: AstObjectValue, outputType: OutputType[_], inputType: InputType[_], selections: Vector[AstSelection]): Option[Map[String, Any]] = {
    val id = UUID.randomUUID().toString
    val objType = outputType
    val objTypeName = outputType match { case OptionType(ObjectType(objName, _, _, _, _, _)) => objName}
    val newObject: Map[String, Any] = objValue.fields.foldLeft(Map[String, Any]("id"-> id))((coll, field) => coll ++ Map(field.name -> resolveInputField(field.value)))
    data((id, objTypeName)) = newObject

    val selectedFields: Vector[String] = selections.collect{case f: AstField => f.name}
    val returnObject = newObject.filterKeys(key => selectedFields.contains(key))
    Some(returnObject)
  }

  def execute(qAst: AstDocument): JsValue = {
    import spray.json._
    import DefaultJsonProtocol._
    var count = 0

    val violations = QueryValidator.default.validateQuery(schema, qAst)
    if (violations.nonEmpty) {
      throw new RuntimeException(s"Query failed validation: $violations")
    }

    val values = AstVisitor.visitAstWithState(schema, qAst, mutable.Map.empty[String, Any]) { (typeInfo, state) ⇒
      AstVisitor.simple(onLeave = {
        case AstOperationDefinition(AstOperationType.Query, _, _, _, Vector(f @ AstField(_, _, _, _, _, _, _, _)), _, _, _) =>
          val outputType = schema.query.fields.find(schemaField => schemaField.name == f.name).get.fieldType
          state("data") = resolveField(f, outputType, Map.empty)
        case AstOperationDefinition(AstOperationType.Mutation, _, _, _, Vector(AstField(_, fieldName, Vector(AstArgument(argName, value, _, _)), _, selections, _, _, _)), _, _, _) =>
          val outputType = schema.mutation.map(m => m.fieldsByName(fieldName).head.fieldType).get
          val inputTypes: InputType[_] =  schema.mutation
            .flatMap(m => m.fieldsByName(fieldName).head
              .arguments.find(_.name == argName)
              .map{ case Argument(_, it, _, _, _, _) => it}).get
          value match {
            case objValue: AstObjectValue =>
              val obj = createObject(objValue, outputType, inputTypes, selections)
              val arg = AstArgument("id", AstStringValue(obj.get("id").asInstanceOf[String]))
              val f = AstField(None, fieldName, Vector(arg), Vector.empty, selections)
              state("data") = resolveField(f, outputType, Map.empty)
            case notImplemented =>
              ???
          }
          count += 1
        case x =>
          println(s"$typeInfo $state, $x")
      })
    }
    println(count)
    values.toMap.toJson
  }
}
