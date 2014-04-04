package eu.swdev.i18n

import org.scalatest.FunSuite

/**
  */
class ResourceTest extends FunSuite {

  @Resource(resourcePath = "abc")
  object Msg {

  }

}

//Apply(
//  Select(
//    Apply(
//      Select(
//        New(
//          Ident(
//            newTypeName("CompiledMessages")
//          )
//        ),
//        nme.CONSTRUCTOR
//      ),
//      List(
//        AssignOrNamedArg(
//          Ident(newTermName("resourcePath")),
//          Literal(Constant("abc"))
//        )
//      )
//    ),
//    newTermName("macroTransform")
//  ),
//  List(ModuleDef(Modifiers(), newTermName("Msg"), Template(List(Select(Ident(scala), newTypeName("AnyRef"))), emptyValDef, List(DefDef(Modifiers(), nme.CONSTRUCTOR, List(), List(List()), TypeTree(), Block(List(Apply(Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR), List())), Literal(Constant(()))))))))
//)
//
