package eu.swdev.i18n

import java.text.MessageFormat
import eu.swdev.config.StringKeyValueTreeModule

/**
 * Transforms messages into markup.
 *
 * Messages that contain markup must be transformed using the markupMsg method whereas messages that contain no
 * markup must be transformed using the rawMsg method.
 */
trait MsgMarkup {

  /**
   * Result type of the transformation.
   *
   * In some situations M may simply be `String`. In other situations M can be a wrapper for strings containing
   * markup. Such a wrapper might be used to indicate to template engines that some output is already markup and
   * needs no escaping.
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

object ResTrees extends StringKeyValueTreeModule {
  type Value = Entry
}