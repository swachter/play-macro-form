package com.abc

import eu.swdev.web.form.Form

@Form
object F {
  val f1 = field[Int].ge(1).le(10)
  val f2 = field[Boolean].addMCheck((errs: Seq[Error], m: Boolean) => if (!m) Error("must be checked") +: errs else errs)
  val f3 = field[Seq[Int]].enum(Seq(1, 2, 3))
  val f4 = field[Int].enum(Seq(1, 2, 3))
  val f5 = field[Option[Int]].enum(Seq(1, 2, 3))
  val f6 = field[String]
  val f7 = field[String].enum(Seq("default", "primary", "success", "info", "warning", "danger", "link"))
  val f8 = field[Double].ge(0).le(1)
}