package eu.swdev.play.i18n

import eu.swdev.i18n.MsgMarkup
import play.api.templates.{HtmlFormat, Html}

/**
  */
object HtmlMarkup extends MsgMarkup {
  /**
   * Transforms a message that contains markup. During the transformation the string value of the message is not changed.
   *
   * @param string
   * @return
   */
  override def markupMsg(string: String): M = Html(string)

  /**
   * Transforms a raw message, i.e. a message that contains no markup. During the transformation certain characters
   * might be escaped. For example in the case of HTML markup the '<' character is transformed into the string "&lt;".
   *
   * @param string
   * @return
   */
  override def rawMsg(string: String): M = HtmlFormat.escape(string)

  override type M = Html
}
