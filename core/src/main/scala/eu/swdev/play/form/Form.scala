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

    // TODO: construct the WFS type alias dynamically
    val wfsTypes = Vector(
      q"type WFS = FS",
      q"type WFS = FS[_]",
      q"type WFS = FS[_, _]",
      q"type WFS = FS[_, _, _]",
      q"type WFS = FS[_, _, _, _]",
      q"type WFS = FS[_, _, _, _, _]",
      q"type WFS = FS[_, _, _, _, _, _]",
      q"type WFS = FS[_, _, _, _, _, _, _]",
      q"type WFS = FS[_, _, _, _, _, _, _, _]",
      q"type WFS = FS[_, _, _, _, _, _, _, _, _]",
      q"type WFS = FS[_, _, _, _, _, _, _, _, _, _]",
      q"type WFS = FS[_, _, _, _, _, _, _, _, _, _, _]"
    )

    abstract class MemberInfo(index: Int, fieldName: TermName) {
      val qFieldName: Tree =  q"$fieldName"
      val strFieldName = fieldName.toString()
      def fvParam: ValDef
      def fsParam: ValDef
      def fsConstraint: TypeDef
      def fillArg: Tree
      val parseArg = q"$fieldName.doParse(name + $strFieldName, view)"
      val modelArg = q"$fieldName.model"

      val constraintTypeName: TypeName = newTypeName(s"C$index")

      def boundedConstraintType(valueType: Tree): TypeDef = {
        q"class X[$constraintTypeName <: Constraints[$valueType, _]]" match {
          case q"class X[$t]" => t
        }
      }

    }

    abstract class FieldInfo(index: Int, fieldName: TermName, valueType: Tree) extends MemberInfo(index, fieldName) {
      val fillArg = q"$fieldName.doFill(name + $strFieldName, model.$fieldName)"
      val fsConstraint = boundedConstraintType(valueType)
    }

    class SimpleFieldInfo(index: Int, fieldName: TermName, valueType: Tree) extends FieldInfo(index, fieldName, valueType) {
      val fvParam = q"val $fieldName: $valueType"
      val fsParam = q"val $fieldName: FieldState[$valueType, $constraintTypeName]"
    }

    class BoxFieldInfo(index: Int, fieldName: TermName, valueType: Tree, boxType: Tree) extends FieldInfo(index, fieldName, valueType) {
      val fvParam = q"val $fieldName: $boxType[$valueType]"
      val fsParam = q"val $fieldName: FieldState[$boxType[$valueType], $constraintTypeName]"
    }

    class FormInfo(index: Int, fieldName: TermName, value: Tree) extends MemberInfo(index, fieldName) {
      val fvParam = q"val $fieldName: $value.FV"
      val fsParam = q"val $fieldName: State[$value.FV]"
      val fsConstraint = q"class X[$constraintTypeName <: Constraints[_, _]]" match { case q"class X[$t]" => t }
      val fillArg = q"$fieldName.doFill(name + $strFieldName, model.$fieldName).asInstanceOf[State[$fieldName.FV]]"
    }

    def processField: PartialFunction[(Tree, Int), MemberInfo] = {
      // if a field with constraints was defined, e.g. field[Int].lt(5);
      // -> remove the last method application and recurse
      case (q"val $fieldName = $a.$f($x)", index) => {
        processField(q"val $fieldName = $a", index)
      }
      case (q"val $fieldName = field[$valueType]", index) => new SimpleFieldInfo(index, fieldName, valueType)
      case (q"val $fieldName = field[$valueType,$boxType]", index) => new BoxFieldInfo(index, fieldName, valueType, boxType)
      case (q"val $fieldName = field2[$boxType[$valueType]]", index) => new BoxFieldInfo(index, fieldName, valueType, boxType)
    }

    val processForm: PartialFunction[(Tree, Int), MemberInfo] = {
      case (q"val $fieldName = $value", index) => new FormInfo(index, fieldName, value)
    }

    val annotteesTrees: List[Tree] = annottees.map(_.tree).toList

    val modDefs: List[Tree] = annotteesTrees.map {
      tree => {
        tree match {
          case q"object $name { ..$body }" => {

            val validations: List[Tree] = body.collect {
              case q"def $f(${_}: FS[..${_}]): Unit = ${_}" => q"$f(this)"
              case q"def $f(${_}: WFS): Unit = ${_}" => q"$f(this)"
            }

            val fInfo: List[MemberInfo] = body.zipWithIndex.collect(processField.orElse(processForm)).asInstanceOf[List[MemberInfo]]

            val fieldNames = fInfo.map(_.qFieldName)
            val fvParams = fInfo.map(_.fvParam)
            val fsParams = fInfo.map(_.fsParam)
            val fsConstraints = fInfo.map(_.fsConstraint)
            val fillArgs = fInfo.map(_.fillArg)
            val parseArgs = fInfo.map(_.parseArg)
            val modelArgs = fInfo.map(_.modelArg)

            // Define the value class that holds the typed value of the form.
            val fvClass = q"case class FV(..$fvParams)"

            // Define the state class of the form.
            // The state class aggregates the states of its nested fields and forms.
            // If there are any validations defined then they are called right in the constructor thereby ensuring
            // that a form state is always validated.
            val fsClass = q"""
            case class FS[..$fsConstraints](..$fsParams) extends eu.swdev.play.form.State[FV] {
              def hasFormErrors = !errors.isEmpty || Seq(..$fieldNames).exists(_.hasFormErrors)
              def hasFieldErrors = Seq(..$fieldNames).exists(_.hasFieldErrors)
              def model = FV(..$modelArgs)
              ..${validations}
            }"""

            val wfsType = wfsTypes(fInfo.size) //q"type WFS = FS[..$constraintTypes]"

            val fillMethod1 = q"def doFill(name: Name, model: FV) = FS(..$fillArgs)"
            val parseMethod1 = q"def doParse(name: Name, view: Map[String, Seq[String]]) = FS(..$parseArgs)"

            val fillMethod2 = q"def fill(model: FV) = doFill(emptyName, model)"
            val parseMethod2 = q"def parse(view: Map[String, Seq[String]]) = doParse(emptyName, view)"

            val tbody = body.asInstanceOf[List[Tree]]

            q"object $name { ..${(fvClass :: fsClass :: wfsType :: fillMethod1 :: parseMethod1 :: fillMethod2 :: parseMethod2 :: tbody).toList} }"
          }
          case x => x
        }
      }
    }
    //println("Block: " + Block(modDefs, Literal(Constant())))

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