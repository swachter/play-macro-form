package com.abc

import eu.swdev.web.form.{FieldState, Form}
import eu.swdev.i18n.Resource
import play.api.i18n.Lang
import eu.swdev.play.form.StdFormResourceApi

@Form
object F {
  val f1 = field[Int].ge(1).le(10)
  val f2 = field[Boolean].addMCheck(m => if (!m) Some(Error("must be checked")) else None)
  val f3 = field[Seq[Int]].enum(Seq(1, 2, 3))
  val f4 = field[Int].enum(Seq(1, 2, 3))
  val f5 = field[Option[Int]].enum(Seq(1, 2, 3))
  val f6 = field[String]
  val f7 = field[Option[String]].enum(Seq("default", "primary", "success", "info", "warning", "danger", "link"))
  val f8 = field[Double].ge(0).le(1)
  val f9 = field[String].enum(Seq("a", "b", "c"))
  val f10 = field[Seq[String]].enum(Seq("a", "b", "c"))
}

@Resource(resourcePath = "form/form-resource")
class FormResourceApiImpl extends StdFormResourceApi