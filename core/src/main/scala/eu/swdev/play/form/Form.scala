package eu.swdev.play.form

import scala.reflect.macros.Context
import scala.language.experimental.macros
import scala.annotation.StaticAnnotation
import scala.reflect.api.Universe

/**
  */
class Form extends StaticAnnotation {
  def macroTransform(annottees: Any*) = macro FormMacro.impl
}

object FormMacro {

  def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {

    import c.universe._

    case class FieldInfo(fieldName: Tree, fvParam: ValDef, fsParam: ValDef, fsConstraint: TypeDef, fillArg: Tree, parseArg: Tree, modelArg: Tree)

    val annotteesTrees: List[Tree] = annottees.map(_.tree).toList

    def constraintInfo(count: Int, valueType: Tree): (TypeName, TypeDef) = {
      val constraintTypeName = newTypeName(s"C$count")
      val constraintType = q"class X[$constraintTypeName <: Constraints[$valueType, _]]" match {
        case q"class X[$t]" => t
      }
      (constraintTypeName, constraintType.asInstanceOf[TypeDef])
    }

    var count = 0

    def processField: PartialFunction[Tree, FieldInfo] = {
      // if a field with constraints was defined, e.g. field[Int].lt(5);
      // -> remove the last method application and recurse
      case q"val $fieldName = $a.$f($x)" => {
        processField(q"val $fieldName = $a")
      }
      case q"val $fieldName = field[$valueType]" => {
        count = count + 1
        val strFieldName = fieldName.toString
        val cInfo = constraintInfo(count, valueType)
        FieldInfo(
          q"$fieldName",
          q"val $fieldName: $valueType",
          q"val $fieldName: FieldState[$valueType, ${cInfo._1}]",
          cInfo._2,
          q"$fieldName.doFill(name + $strFieldName, model.$fieldName)",
          q"$fieldName.doParse(name + $strFieldName, view)",
          q"$fieldName.model"
        )
      }
      case q"val $fieldName = field[$valueType,$boxType]" => {
        count = count + 1
        val strFieldName = fieldName.toString
        val cInfo = constraintInfo(count, valueType)
        FieldInfo(
          q"$fieldName",
          q"val $fieldName: $boxType[$valueType]",
          q"val $fieldName: FieldState[$boxType[$valueType], ${cInfo._1}]",
          cInfo._2,
          q"$fieldName.doFill(name + $strFieldName, model.$fieldName)",
          q"$fieldName.doParse(name + $strFieldName, view)",
          q"$fieldName.model"
        )
      }
    }

    def processForm: PartialFunction[Tree, FieldInfo] = {
      case q"val $fieldName = $value" => {
        count = count + 1
        val strFieldName = fieldName.toString
        val constraintTypeName = newTypeName(s"C$count")
        val constraintType = q"class X[$constraintTypeName <: Constraints[_, _]]" match {
          case q"class X[$t]" => t
        }
        FieldInfo(
          q"$fieldName",
          q"val $fieldName: $value.FV",
          q"val $fieldName: State[$value.FV]",
          constraintType,
          q"$fieldName.doFill(name + $strFieldName, model.$fieldName).asInstanceOf[State[$fieldName.FV]]",
          q"$fieldName.doParse(name + $strFieldName, view)",
          q"$fieldName.model"
        )
      }
    }

    val modDefs: List[Tree] = annotteesTrees.map {
      tree => tree match {
        case q"object $name { ..$body }" => {
          val tbody = body.asInstanceOf[List[Tree]]
          val fInfo: List[FieldInfo] = body.collect(processField.orElse(processForm)).asInstanceOf[List[FieldInfo]]

          val fieldNames = fInfo.map(_.fieldName)
          val fvParams = fInfo.map(_.fvParam)
          val fsParams = fInfo.map(_.fsParam)
          val fsConstraints = fInfo.map(_.fsConstraint)
          val fillArgs = fInfo.map(_.fillArg)
          val parseArgs = fInfo.map(_.parseArg)
          val modelArgs = fInfo.map(_.modelArg)

          // define a value class that can hold the typed values of the form
          val fvClass = q"case class FV(..$fvParams)"

          // define a state class for the form
          val fsClass = q"""
          case class FS[..$fsConstraints](..$fsParams) extends eu.swdev.play.form.State[FV] {
            def hasErrors = !errors.isEmpty || Seq(..$fieldNames).exists(_.hasErrors)
            def model = FV(..$modelArgs)
          }"""

          val fillMethod1 = q"def doFill(name: Name, model: FV) = FS(..$fillArgs)"
          val parseMethod1 = q"def doParse(name: Name, view: Map[String, Seq[String]]) = FS(..$parseArgs)"
          val fillMethod2 = q"def fill(model: FV) = doFill(emptyName, model)"
          val parseMethod2 = q"def parse(view: Map[String, Seq[String]]) = doParse(emptyName, view)"

          q"object $name { ..${ (fvClass :: fsClass :: fillMethod1 :: parseMethod1 :: fillMethod2 :: parseMethod2 :: tbody).toList} }"
        }
        case x => x
      }
    }
    c.Expr(Block(modDefs, Literal(Constant())))
  }

}

class ReflectionUtil(val universe: Universe) {
  import universe._

  def valMembers(typ: Type): List[TermSymbol] = {
    typ.members.iterator.filter(_.isTerm).map(_.asTerm).filter(_.isVal).toList
  }

  def printAllValues(typ: Type): Unit = {

    valMembers(typ).foreach(ts => println(s"ts: $ts; typeSignature: ${ts.typeSignature}; typeSignatureIn: ${ts.typeSignatureIn(typ)}; class: ${ts.getClass}"))

  }
}