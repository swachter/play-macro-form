package eu.swdev.i18n

import java.text.MessageFormat
import eu.swdev.config.StringKeyValueTreeModule

/**
 * A message format that knows if it contains markup or not.
 *
 * @param format
 * @param isMarkup Indicates if the message contains markup or not.
 */
case class MsgFormat(format: MessageFormat, isMarkup: Boolean) {

  /**
   * Format the message and return the raw message text.
   *
   * @param args
   * @return
   */
  def rawMsg(args: Array[Object]): String = {
    format.format(args)
  }

  /**
   * Format the message and return it as markup.
   *
   * @param args
   * @param markup
   * @return
   */
  def markupMsg(args: Array[Object])(implicit markup: Markup): markup.M = {
    val s = format.format(args)
    if (isMarkup) markup.markupMsg(s) else markup.rawMsg(s)
  }

}

/**
 * Transforms messages into markup.
 *
 * Messages that contain markup must be transformed using the markupMsg method whereas messages that contain no
 * markup must be transformed using the rawMsg method.
 */
trait Markup {

  /**
   * Result type of the transformation.
   *
   * In some situations M may simply be `String`. In other situations M can be a wrapper for strings containing
   * markup. In this situation the wrapper can indicate to a template engine that its content is already markup and
   * needs not to be escaped.
   */
  type M

  /**
   * Transforms a raw message, i.e. a message that contains no markup. During the transformation certain characters
   * might be escaped. For example in the case of HTML markup the '<' character is transformed into the string "&lt;".
   *
   * @param string
   * @return
   */
  def rawMsg(string: String): M

  /**
   * Transforms a message that contains markup. During the transformation the string value of the message is not changed.
   *
   * @param string
   * @return
   */
  def markupMsg(string: String): M
}

/**
 * A module for key-value trees that contain message formats.
 */
object MsgLookup extends StringKeyValueTreeModule {
  type Value = MsgFormat
}
