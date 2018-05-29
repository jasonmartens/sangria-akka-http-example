import java.util.UUID

import sangria.ast.{AstVisitor, BigIntValue, ObjectTypeDefinition, Argument => AstArgument, Document => AstDocument, Field => AstField, FieldDefinition => AstFieldDefinition, ListValue => AstListValue, NameValue => AstNameValue, ObjectField => AstObjectField, ObjectValue => AstObjectValue, OperationDefinition => AstOperationDefinition, OperationType => AstOperationType, ScalarValue => AstScalarValue, Selection => AstSelection, StringValue => AstStringValue, TypeDefinition => AstTypeDefinition, Value => AstValue}
import sangria.execution.Executor
import sangria.parser.QueryParser
import sangria.schema._
import sangria.validation.QueryValidator

import scala.concurrent.{ExecutionContext, Future}

/**
  * Defines a GraphQL schema for the current project
  */
object SchemaDefinition {

  def generateSchema(schemaSDL: String): Schema[Any, Any] = {
    Schema.buildFromAst(QueryParser.parse(schemaSDL).get)
  }

  def resolveField(astField: AstField, outputType: OutputType[_], obj: Map[String, Any]): Map[String, Any] = {
    outputType match {
      case ScalarType("ID", _, _, _, _, _, _, _, _) =>
        Map(astField.name -> obj(astField.name))
      case ScalarType("String", _, _, _, _, _, _, _, _) =>
        Map(astField.name -> obj(astField.name))
      case ListType(objType @ ObjectType(_, _, _, _, _, _, _)) =>
        val objectIDs = obj.getOrElse(astField.name, Vector()).asInstanceOf[Vector[String]]
        val objects = objectIDs.map { id =>
          resolveObject(id, objType, astField.selections)
        }
        Map(astField.name -> objects)
      case OptionType(objType@ObjectType(objName, _, _, _, _, _, _)) =>
        val idField = astField.arguments.find(_.name == "id")
        idField match {
          case Some(AstArgument(_, BigIntValue(intVal, _, _), _, _)) =>
            val obj = resolveObject(intVal.toString(), objType, astField.selections)
            Map(astField.name -> obj)
          case Some(AstArgument(_, AstStringValue(value, _, _, _, _), _, _)) =>
            val obj = resolveObject(value, objType, astField.selections)
            Map(astField.name -> obj)
          case None =>
            Map.empty
          case x => throw new NotImplementedError(s"$x is not implemented")
        }
      case unsupported =>
        ???
    }
  }

  def resolveObject(id: String, objectType: ObjectType[_, _], selections: Vector[AstSelection]): Map[String, Any] = {
    val selectedFields: Vector[String] = selections.map {
      case f: AstField => f.name
      case _ => ???
    }
    Data.lookupByIdAndType(id, objectType.name) match {
      case Some(obj) =>
        obj.filterKeys(k => selectedFields.contains(k)).map { obj => }
        val objFields = selections.map {
          case selectedField: AstField =>
            val schemaField = objectType.fields.find(_.name == selectedField.name).get
            resolveField(selectedField, schemaField.fieldType, obj)
          case _ => ???
        }.foldLeft(Map.empty[String, Any])((coll, map) => coll ++ map)
        objFields
      case None => Map.empty
      case _ => ???
    }
  }

  def resolveInputField(value: AstValue): Any = {
    value match {
      case AstObjectValue(fields, _, _) =>
        fields.foldLeft(Map.empty[String, Any])((coll, f) => coll ++ Map(f.name -> resolveInputField(f.value)))
      case objField: AstObjectField =>
        Map(objField.name -> resolveInputField(objField.value))
      case AstStringValue(v, _, _, _, _) =>
        v
      case AstListValue(vs, _, _) =>
        vs.map(resolveInputField)
      case _ =>
        ???
    }
  }

  def createObject(objValue: AstObjectValue, outputType: OutputType[_], inputType: InputType[_], selections: Vector[AstSelection]): Option[Map[String, Any]] = {
    val id = UUID.randomUUID().toString
    val objTypeName = outputType match {
      case OptionType(ObjectType(objName, _, _, _, _, _, _)) => objName
      case _ => ???
    }
    val newObject: Map[String, Any] = objValue.fields.foldLeft(Map[String, Any]("id"-> id))((coll, field) => coll ++ Map(field.name -> resolveInputField(field.value)))
    Data.addObject(id, objTypeName, newObject)

    val selectedFields: Vector[String] = selections.collect{case f: AstField => f.name}
    val returnObject = newObject.filterKeys(key => selectedFields.contains(key))
    Some(returnObject)
  }

  def executeQuery(schema: Schema[_, _], field: AstField): Map[String, Any] = {
    val outputType = schema.query.fields.find(schemaField => schemaField.name == field.name).get.fieldType
    resolveField(field, outputType, Map.empty)
  }

  def executeMutation(schema: Schema[_, _], fieldName: String, argName: String, argValue: AstValue, selections: Vector[AstSelection]): Any = {
    val outputType = schema.mutation.map(m => m.fieldsByName(fieldName).head.fieldType).get
    val inputTypes: InputType[_] = schema.mutation
      .flatMap(
        m => m.fieldsByName(fieldName).head
          .arguments.find(_.name == argName)
          .map { case Argument(_, it, _, _, _, _, _) => it }
      ).get
    argValue match {
      case objValue: AstObjectValue =>
        val obj = createObject(objValue, outputType, inputTypes, selections)
        val arg = AstArgument("id", AstStringValue(obj.get("id").asInstanceOf[String]))
        val f = AstField(None, fieldName, Vector(arg), Vector.empty, selections)
        resolveField(f, outputType, Map.empty)
      case _ =>
        ???
    }
  }

  def execute(qAst: AstDocument, schema: Schema[Any, Any])(implicit ec: ExecutionContext): Future[Any] = {
    val violations = QueryValidator.default.validateQuery(schema, qAst)
    if (violations.nonEmpty) {
      throw new RuntimeException(s"Query failed validation: $violations")
    }

    val result = qAst match {
      case AstDocument(Vector(AstOperationDefinition(AstOperationType.Query, _, _, _, Vector(field @ AstField(_, _, _, _, _, _, _, _)), _, _, _)), _, _, _) =>
        Future.successful(Map("data" -> executeQuery(schema, field)))
      case AstDocument(Vector(AstOperationDefinition(AstOperationType.Mutation, _, _, _, Vector(AstField(_, fieldName, Vector(AstArgument(argName, argValue, _, _)), _, selections, _, _, _)), _, _, _)), _, _, _) =>
        Future.successful(Map("data" -> executeMutation(schema, fieldName, argName, argValue, selections)))
      case _ =>
        Executor.execute(schema, qAst)
    }

    result
  }
}
