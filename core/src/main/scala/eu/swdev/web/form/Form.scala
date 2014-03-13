package eu.swdev.web.form

import scala.reflect.macros.Context
import scala.language.experimental.macros
import scala.annotation.StaticAnnotation
import scala.reflect.api.Universe

/** The Form annotation is a macro annotation that can be applied on objects that describe input forms.
  *
  * A form is described by an immutable object that contains "val" members for fields and referenced nested form
  * descriptions.
  *
  * A form description can be used to "parse" a Map[String, Seq[String]] into a so called form state. A form state
  * contains for each directly or indirectly contained field the Seq[String] value (from the input map),
  * any conversion errors that occurred when the Seq[String] was converted into a typed value,
  * and possibly the converted typed value of the field. A form state can also be produced when by "filling" an
  * existing typed value for the form into the form.
  *
  * The Form annotation augments the annotated object with two case classes named FS and FV that represent the form state
  * and form value, respectively. In addition the two methods "def parse(view: Map[String, Seq[String]]): FS" and
  * "def fill(model: FV): FS" are created.
  *
  * The form state contains the information that is necessary to render the form. The form state indicates if there
  * were conversion or validation errors. If there were no such errors then the form value can be extracted by the
  * "def model: FV" method.
  */
class Form extends StaticAnnotation {
  def macroTransform(annottees: Any*) = macro FormMacro.impl
}

object FormMacro {

  def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {

    import c.universe._

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
      def fillArg = q"$memberName.fill(model.$memberName, validationArg, name + $strFieldName)"
      val parseArg = q"$memberName.parse(view, validationArg, name + $strFieldName)"
      val modelArg = q"$memberName._model"
      val constraintTypeName: TypeName = newTypeName(s"C$index")
    }

    abstract class FieldInfo(index: Int, memberName: TermName, objectName: TermName) extends SpliceInfo(index, memberName) {
      val fsParam = q"val $memberName: FieldState[$objectName.$memberName.V, $objectName.$memberName.M, $objectName.$memberName.F]"
    }

    class SimpleFieldInfo(index: Int, memberName: TermName, valueType: Tree, objectName: TermName) extends FieldInfo(index, memberName, objectName) {
      val fvParam = q"val $memberName: $valueType"
    }

    class BoxFieldInfo(index: Int, memberName: TermName, valueType: Tree, boxType: Tree, objectName: TermName) extends FieldInfo(index, memberName, objectName) {
      val fvParam = q"val $memberName: $boxType[$valueType]"
    }

    class FormInfo(index: Int, memberName: TermName, value: Tree) extends SpliceInfo(index, memberName) {
      val fvParam = q"val $memberName: $value.FV"
      val fsParam = q"val $memberName: $value.FS"
    }

    def processField: PartialFunction[(Tree, TermName, Int), SpliceInfo] = {
      // if a field with constraints was defined, e.g. field[Int].lt(5);
      // -> remove the last method application and recurse
      case (q"val $fieldName = $a.$f($x)", objectName, index) => processField(q"val $fieldName = $a", objectName, index)
      case (q"val $fieldName = field[$boxType[$valueType]]", objectName, index) => new BoxFieldInfo(index, fieldName, valueType, boxType, objectName)
      case (q"val $fieldName = field[$valueType]", objectName, index) => new SimpleFieldInfo(index, fieldName, valueType, objectName)
    }

    val processForm: PartialFunction[(Tree, TermName, Int), SpliceInfo] = {
      case (q"val $fieldName = $value", _, index) => new FormInfo(index, fieldName, value)
    }

    val annotteesTrees: List[Tree] = annottees.map(_.tree).toList

    val modDefs: List[Tree] = annotteesTrees.map {
      tree => {
        tree match {
          case q"object $objectName { ..$body }" => {

            // collect calls to validation methods, i.e. methods with the signature FS[..] => Unit or WFS => Unit
            val validations: List[Tree] = body.collect {
              case q"def $f(${_}: FS): Option[Error] = ${_}" => q"$f(this)"
            }

            val spliceInfos: List[SpliceInfo] = body.zipWithIndex.map(t => (t._1, objectName, t._2)).collect(processField.orElse(processForm)).asInstanceOf[List[SpliceInfo]]

            val memberNames = spliceInfos.map(_.qMemberName)
            val fvParams = spliceInfos.map(_.fvParam)
            val fsParams = spliceInfos.map(_.fsParam)
            val fillArgs = spliceInfos.map(_.fillArg)
            val parseArgs = spliceInfos.map(_.parseArg)
            val modelArgs = spliceInfos.map(_.modelArg)

            val tbody = body.asInstanceOf[List[Tree]]

            val objectStrName = objectName.decoded

            // output the modified object
            q"""
            object $objectName {

                  import eu.swdev.web.form._

                  def fill(model: FV, validationArg: Validation = WithValidation, name: Name = Name($objectStrName)) = {
                    FS(name, ..$fillArgs)(validationArg)
                  }
                  def parse(view: Map[String, Seq[String]], validationArg: Validation = WithValidation, name: Name = Name($objectStrName)) = {
                    FS(name, ..$parseArgs)(validationArg)
                  }

                  case class FV(..$fvParams)

                  case class FS(_name: Name, ..$fsParams)(val validationArg: Validation) extends FormState[FV] {
                    val _errors = if (hasFieldErrors) Nil else validationArg.validate(Nil, Seq[Option[Error]](..${validations}).foldLeft(List.empty[Error])((a, o) => o.map(_ :: a).getOrElse(a)))
                    def hasFormErrors = !_errors.isEmpty || Seq[State[_]](..$memberNames).exists(_.hasFormErrors)
                    def collectFormErrors(accu: Seq[eu.swdev.web.form.Error]) = Seq(..$memberNames).foldLeft(if (_errors.isEmpty) accu else _errors ++ accu)((a, m) => m.collectFormErrors(a))
                    def hasFieldErrors = Seq[State[_]](..$memberNames).exists(_.hasFieldErrors)
                    def _model = FV(..$modelArgs)
                  }

                  ..$tbody
            }"""
          }
          case x => x
        }
      }
    }
    //println("Block: " + Block(modDefs, Literal(Constant())))

    c.Expr(Block(modDefs, Literal(Constant())))
  }

}

//class ReflectionUtil(val universe: Universe) {
//
//  import universe._
//
//  def valMembers(typ: Type): List[TermSymbol] = {
//    typ.members.iterator.filter(_.isTerm).map(_.asTerm).filter(_.isVal).toList
//  }
//
//  def printAllValues(typ: Type): Unit = {
//
//    valMembers(typ).foreach(ts => println(s"ts: $ts; typeSignature: ${ts.typeSignature}; typeSignatureIn: ${ts.typeSignatureIn(typ)}; class: ${ts.getClass}"))
//
//  }
//}