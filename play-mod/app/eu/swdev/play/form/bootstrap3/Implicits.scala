package eu.swdev.play.form.bootstrap3

import eu.swdev.web.form.{FormState, FieldState, FieldFeatures}
import eu.swdev.web.style._
import play.api.i18n.Lang
import eu.swdev.web.EntryPoint
import scala.language.implicitConversions

/**
 * Contains bootstrap3-form-related implicit conversions.
 */
trait Implicits extends eu.swdev.play.Implicits {

  implicit def `eu.swdev.web.form.FieldState-> eu.swdev.play.form.bootstrap3.FieldRendererImpl`[V, M, CS <: FieldFeatures](fieldState: FieldState[V, M, CS])(implicit style: Style, lang: Lang) = new FieldRendererImpl(fieldState)

  implicit def `eu.swdev.web.form.FieldState-> eu.swdev.play.form.bootstrap3.FieldWithStyle`[V, M, CS <: FieldFeatures](fieldState: FieldState[V, M, CS])(implicit style: Style, lang: Lang) = new FieldWithStyle(fieldState)

  implicit def `eu.swdev.web.form.FormState-> eu.swdev.play.form.bootstrap3.FormRendererImpl`[M](formState: FormState[M])(implicit style: Style, lang: Lang) = new FormRendererImpl(formState)

  implicit def `eu.swdev.web.form.FormState-> eu.swdev.play.form.bootstrap3.FormWithStyle`[M](formState: FormState[M])(implicit style: Style, lang: Lang) = new FormWithStyle(formState)

  implicit def `eu.swdev.web.form.FieldState-> eu.swdev.play.form.bootstrap3.FieldAttrs`[V, M, CS <: FieldFeatures](fieldState: FieldState[V, M, CS]) = new FieldAttrs(fieldState)

  /**
   * Allows to access the members of the StyledItems object via the entry point object.
   *
   * @param entryPoint
   * @return
   */
  implicit def `eu.swdev.web.EntryPoint->eu.swdev.play.form.bootstrap3.StyledItems`(entryPoint: EntryPoint.type) = StyledItems

}
