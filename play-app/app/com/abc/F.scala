package com.abc

import eu.swdev.web.form.Form

@Form
object F {
  val f1 = field[Int]
  val f2 = field[Boolean].addMCheck((errs: Seq[String], m: Boolean) => if (!m) "must be checked" +: errs else errs)
}