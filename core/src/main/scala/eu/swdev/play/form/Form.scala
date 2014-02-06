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

    /**
     * Contains parts that are spliced into the form object.
     * 
     * Depending on the member kind (i.e. if it is a field or references another form) some of the spliced parts have
     * different values.
     * 
     * @param index The index of the member in its containing object. The index is used to construct unique constraint type
     *              names, i.e. C0, C1, ...
     * @param memberName
     */
    abstract class SpliceInfo(index: Int, memberName: TermName) {
      val qMemberName: Tree =  q"$memberName"
      val strFieldName = memberName.toString()
      def fvParam: ValDef
      def fsParam: ValDef
      def fsConstraint: TypeDef
      def fillArg: Tree
      val parseArg = q"$memberName.doParse(name + $strFieldName, view)"
      val modelArg = q"$memberName.model"
      val constraintTypeName: TypeName = newTypeName(s"C$index")
    }

    abstract class FieldInfo(index: Int, memberName: TermName, valueType: Tree) extends SpliceInfo(index, memberName) {
      val fillArg = q"$memberName.doFill(name + $strFieldName, model.$memberName)"
      val fsConstraint = q"class X[$constraintTypeName <: Constraints[$valueType, _]]" match { case q"class X[$t]" => t }
    }

    class SimpleFieldInfo(index: Int, memberName: TermName, valueType: Tree) extends FieldInfo(index, memberName, valueType) {
      val fvParam = q"val $memberName: $valueType"
      val fsParam = q"val $memberName: FieldState[$valueType, $constraintTypeName]"
    }

    class BoxFieldInfo(index: Int, memberName: TermName, valueType: Tree, boxType: Tree) extends FieldInfo(index, memberName, valueType) {
      val fvParam = q"val $memberName: $boxType[$valueType]"
      val fsParam = q"val $memberName: FieldState[$boxType[$valueType], $constraintTypeName]"
    }

    class FormInfo(index: Int, memberName: TermName, value: Tree) extends SpliceInfo(index, memberName) {
      val fvParam = q"val $memberName: $value.FV"
      val fsParam = q"val $memberName: State[$value.FV]"
      val fsConstraint = q"class X[$constraintTypeName <: Constraints[_, _]]" match { case q"class X[$t]" => t }
      val fillArg = q"$memberName.doFill(name + $strFieldName, model.$memberName).asInstanceOf[State[$memberName.FV]]"
    }

    def processField: PartialFunction[(Tree, Int), SpliceInfo] = {
      // if a field with constraints was defined, e.g. field[Int].lt(5);
      // -> remove the last method application and recurse
      case (q"val $fieldName = $a.$f($x)", index) => processField(q"val $fieldName = $a", index)
      case (q"val $fieldName = field[$boxType[$valueType]]", index) => new BoxFieldInfo(index, fieldName, valueType, boxType)
      case (q"val $fieldName = field[$valueType,$boxType]", index) => new BoxFieldInfo(index, fieldName, valueType, boxType)
      case (q"val $fieldName = field[$valueType]", index) => new SimpleFieldInfo(index, fieldName, valueType)
      case (q"val $fieldName = field2[$boxType[$valueType]]", index) => new BoxFieldInfo(index, fieldName, valueType, boxType)
    }

    val processForm: PartialFunction[(Tree, Int), SpliceInfo] = {
      case (q"val $fieldName = $value", index) => new FormInfo(index, fieldName, value)
    }

    val annotteesTrees: List[Tree] = annottees.map(_.tree).toList

    val modDefs: List[Tree] = annotteesTrees.map {
      tree => {
        tree match {
          case q"object $name { ..$body }" => {

            // collect calls to validation methods, i.e. methods with the signature FS[..] => Unit or WFS => Unit
            val validations: List[Tree] = body.collect {
              case q"def $f(${_}: FS[..${_}]): Unit = ${_}" => q"$f(this)"
              case q"def $f(${_}: WFS): Unit = ${_}" => q"$f(this)"
            }

            val spliceInfos: List[SpliceInfo] = body.zipWithIndex.collect(processField.orElse(processForm)).asInstanceOf[List[SpliceInfo]]

            val memberNames = spliceInfos.map(_.qMemberName)
            val fvParams = spliceInfos.map(_.fvParam)
            val fsParams = spliceInfos.map(_.fsParam)
            val fsConstraints = spliceInfos.map(_.fsConstraint)
            val fillArgs = spliceInfos.map(_.fillArg)
            val parseArgs = spliceInfos.map(_.parseArg)
            val modelArgs = spliceInfos.map(_.modelArg)

            // Define the value class that holds the typed value of the form.
            val fvClass = q"case class FV(..$fvParams)"

            // Define the state class of the form.
            // The state class aggregates the states of its nested fields and forms.
            // If there are any validations defined then they are called right in the constructor thereby ensuring
            // that a form state is always validated.
            val fsClass = q"""
            case class FS[..$fsConstraints](..$fsParams) extends eu.swdev.play.form.State[FV] {
              def hasFormErrors = !errors.isEmpty || Seq(..$memberNames).exists(_.hasFormErrors)
              def hasFieldErrors = Seq(..$memberNames).exists(_.hasFieldErrors)
              def model = FV(..$modelArgs)
              ..${validations}
            }"""

            val wfsType = wfsTypes(spliceInfos.size) //q"type WFS = FS[..$constraintTypes]"

            val fillMethod1 = q"def doFill(name: Name, model: FV) = FS(..$fillArgs)"
            val parseMethod1 = q"def doParse(name: Name, view: Map[String, Seq[String]]) = FS(..$parseArgs)"

            val fillMethod2 = q"def fill(model: FV) = doFill(Name.empty, model)"
            val parseMethod2 = q"def parse(view: Map[String, Seq[String]]) = doParse(Name.empty, view)"

            val tbody = body.asInstanceOf[List[Tree]]

            // output the modified object definition by inserting various parts
            q"object $name { ..${fvClass :: fsClass :: wfsType :: fillMethod1 :: parseMethod1 :: fillMethod2 :: parseMethod2 :: tbody} }"
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