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

  def implx(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._
    val inputs = annottees.map(_.tree).toList
    val (annottee, expandees) = inputs match {
      case (param: ValDef) :: (rest@(_ :: _)) => (param, rest)
      case (param: TypeDef) :: (rest@(_ :: _)) => (param, rest)
      case _ => (EmptyTree, inputs)
    }
    println((annottee, expandees))
    val outputs = expandees
    c.Expr[Any](Block(outputs, Literal(Constant(()))))
  }

  def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    val annotteesTrees: List[Tree] = annottees.map(_.tree).toList

    def constraintInfo(count: Int, valueType: Tree): (TypeName, TypeDef) = {
      val constraintTypeName = newTypeName(s"C$count")
      val constraintType = q"class X[$constraintTypeName <: Constraints[$valueType, _]]" match {
        case q"class X[$t]" => t
      }
      (constraintTypeName, constraintType.asInstanceOf[TypeDef])
    }

    val modDefs: List[Tree] = annotteesTrees.map {
      tree => tree match {
        case q"object $name { ..$body }" =>
          val tbody = body.asInstanceOf[List[Tree]]
          var count = 0;
          val fInfo: List[(Tree, ValDef, ValDef, TypeDef, Tree, Tree, Tree)] = body.collect {
            case q"val $fieldName = field[$valueType]" => {
              count = count + 1
              val strFieldName = fieldName.toString
              val cInfo = constraintInfo(count, valueType)
              (
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
              (
                q"$fieldName",
                q"val $fieldName: $boxType[$valueType]",
                q"val $fieldName: FieldState[$boxType[$valueType], ${cInfo._1}]",
                cInfo._2,
                q"$fieldName.doFill(name + $strFieldName, model.$fieldName)",
                q"$fieldName.doParse(name + $strFieldName, view)",
                q"$fieldName.model"
              )
            }
            case q"val $fieldName = $value" => {
              count = count + 1
              val strFieldName = fieldName.toString
              val constraintTypeName = newTypeName(s"C$count")
              val constraintType = q"class X[$constraintTypeName <: Constraints[_, _]]" match {
                case q"class X[$t]" => t
              }
              (
                q"$fieldName",
                q"val $fieldName: $value.FV",
                q"val $fieldName: State[$value.FV]",
                constraintType,
                q"$fieldName.doFill(name + $strFieldName, model.$fieldName).asInstanceOf[State[$fieldName.FV]]",
                q"$fieldName.doParse(name + $strFieldName, view)",
                q"$fieldName.model"
              )
            }
          }.asInstanceOf[List[(Tree, ValDef, ValDef, TypeDef, Tree, Tree, Tree)]]

          val fieldNames = fInfo.map(_._1)
          val fvParams = fInfo.map(_._2)
          val fsParams = fInfo.map(_._3)
          val fsConstraints = fInfo.map(_._4)
          val fillArgs = fInfo.map(_._5)
          val parseArgs = fInfo.map(_._6)
          val modelArgs = fInfo.map(_._7)

          // define a value class that can hold the typed values of the form
          val fvClass = q"case class FV(..$fvParams)"

          // define a state class for the form
          val fsClass = q"""case class FS[..$fsConstraints](..$fsParams) extends eu.swdev.play.form.State[FV] {
            def hasErrors = !errors.isEmpty || Seq(..$fieldNames).exists(_.hasErrors)
            def model = FV(..$modelArgs)
          }"""

          val fillMethod1 = q"def doFill(name: Name, model: FV) = FS(..$fillArgs)"
          val parseMethod1 = q"def doParse(name: Name, view: Map[String, Seq[String]]) = FS(..$parseArgs)"
          val fillMethod2 = q"def fill(model: FV) = doFill(emptyName, model)"
          val parseMethod2 = q"def parse(view: Map[String, Seq[String]]) = doParse(emptyName, view)"

          q"object $name { ..${ (fvClass :: fsClass :: fillMethod1 :: parseMethod1 :: fillMethod2 :: parseMethod2 :: tbody).toList} }"
        case x =>
          x
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