package eu.swdev.i18n

import java.text.MessageFormat
import scala.io.Source
import scala.util.parsing.combinator.RegexParsers
import java.net.URL
import scala.util.parsing.input.StreamReader

import scala.language.postfixOps
import scala.collection.GenTraversable


/**
 * Identifies an entry line.
 *
 * The entry id represents everything that is left to the assignment operator in an entry line.
 */
sealed trait EntryId {
  /**
   * The name of an entry. In case of tree-valued or map-valued entries the same entry name appears on several entry lines.
   * In that case all the lines belonging to the same entry are used together to build the corresponding tree or map.
   *
   * @return
   */
  def name: EntryName
}

/**
 * Identifies an entry line belonging to an entry that is either a message entry or a reference to another entry.
 *
 * @param name
 */
case class SimpleEntryId(name: EntryName) extends EntryId

/**
 * Identifies an entry line belonging to a tree-valued entry.
 *
 * @param name the name of the entry
 * @param key a dot separated string that represents a path in the tree; the entry line defines the value for this path
 */
case class TreeEntryId(name: EntryName, key: String) extends EntryId

/**
 * Identifies an entry line belonging to a map-valued entry.
 *
 * @param name the name of the entry
 * @param key the key for which this entry line defines a value
 */
case class MapEntryId(name: EntryName, key: String) extends EntryId

sealed trait EntryValue

case class MsgEntryValue(format: MessageFormat, isMarkup: Boolean) extends EntryValue

case class LinkEntryValue(name: EntryName) extends EntryValue

/**
 * Represents a logical entry line in a resource file.
 * 
 * A logical entry line typically corresponds to a single natural line. However, if the entry line has a message value
 * then the message value can span several natural lines by ending a line with a (non-escaped) backslash character.
 * In that case the line break and any whitespace characters at the beginning of the following line are ignored.
 *
 * @param id the identifier of the entry;
 * @param value
 */
case class EntryLine(id: EntryId, value: EntryValue)

sealed trait ResType {
  def args: Int
  def isMarkup: Boolean
}

sealed trait LookupResType extends ResType

/**
 * Describes an entry that corresponds to one or more message formats.
 * 
 * @param args the maximum number of arguments a message has
 * @param isMarkup indicates if the message contains markup or not
 */
case class MsgResType(args: Int, isMarkup: Boolean) extends ResType

case class TreeResType(nested: ResType) extends LookupResType {
  def args: Int = nested.args
  def isMarkup: Boolean = nested.isMarkup
}

case class MapResType(nested: ResType) extends LookupResType {
  def args: Int = nested.args
  def isMarkup: Boolean = nested.isMarkup
}

object MsgResType {
  def apply(format: MessageFormat, isMarkup: Boolean): MsgResType = MsgResType(format.getFormatsByArgumentIndex.length, isMarkup)
}

//
//
//

sealed trait ResValue {

  def isMsg: Boolean
  def isTreeOrMap: Boolean

  /**
   * Format the message and return the raw message text.
   *
   * This method must be called only if this ResValue is a MsgResValue.
   *
   * @param args
   * @return
   */
  def outputRaw(args: Array[Object]): String

  /**
   * Format the message and return it as markup.
   *
   * This method must be called only if this ResValue is a MsgResValue.
   *
   * @param args
   * @param markup
   * @return
   */
  def outputMarkup(args: Array[Object])(implicit markup: MsgMarkup): markup.M

  /**
   * Lookup a resource value using the specified key.
   *
   * This method must be called only if this ResValue is a TreeResValue or MapResValue.
   *
   * @param key
   * @return
   */
  def lookup(key: String): Option[ResValue]

}

/**
 *
 * @param format
 * @param isMarkup indicates if the format contains markup or not
 */
case class MsgResValue(format: MessageFormat, isMarkup: Boolean) extends ResValue {
  override def isTreeOrMap = false
  override def isMsg = true
  override def lookup(key: String): Option[ResValue] = throw new UnsupportedOperationException

  override def outputRaw(args: Array[Object]): String = {
    format.format(args)
  }

  override def outputMarkup(args: Array[Object])(implicit markup: MsgMarkup): markup.M = {
    val s = format.format(args)
    if (isMarkup) markup.markupMsg(s) else markup.rawMsg(s)
  }

}

case class TreeResValue(tree: ResTrees.KeyValueTree) extends ResValue {
  override def isTreeOrMap = true
  override def isMsg = false
  override def lookup(key: String): Option[ResValue] = tree.getValue(key)
  override def outputRaw(args: Array[Object]): String = throw new UnsupportedOperationException
  override def outputMarkup(args: Array[Object])(implicit markup: MsgMarkup): markup.M = throw new UnsupportedOperationException
}

case class MapResValue(map: Map[String, ResValue]) extends ResValue {
  override def isTreeOrMap = true
  override def isMsg = false
  override def lookup(key: String): Option[ResValue] = map.get(key)
  override def outputRaw(args: Array[Object]): String = throw new UnsupportedOperationException
  override def outputMarkup(args: Array[Object])(implicit markup: MsgMarkup): markup.M = throw new UnsupportedOperationException
}

/**
 *
 * @param entries The sequential arrangement of the entries correspond to their appearance in definitions. Entries that
 *                were defined earlier are in front of entries that were defined later. If an entry is overwritten
 *                by a later definition then its earlier appearance is removed.
 */
case class ResourceEntries private (entries: List[EntryLine]) {

  def add(other: ResourceEntries): ResourceEntries = this ++ other.entries

  def ++(other: GenTraversable[EntryLine]): ResourceEntries = {
    val zero = (List.empty[EntryLine], Set.empty[EntryId])
    val t = (entries ++ other).foldRight(zero)((e, accu) => if (accu._2.contains(e.id)) accu else (e :: accu._1, accu._2 + e.id))
    ResourceEntries(t._1)
  }
}

object ResourceEntries {

  val empty = new ResourceEntries(Nil)

  def apply(url: URL): ResourceEntries = {
    val entries = ResourceParser.parse(url)
    empty ++ entries
  }

  def apply(urls: TraversableOnce[URL]): ResourceEntries = {
    val lines = for {
      url <- urls
    } yield {
      ResourceParser.parse(url)
    }
    empty ++ lines.toList.flatten
  }

  object ResourceParser extends RegexParsers {

    override def skipWhitespace = false
    override val whiteSpace = """[ \t]+""".r

    def ws = opt(whiteSpace)
    def newLine = namedError((("\r"?) ~> "\n"), "End of line expected")

    val entryName: Parser[String] = namedError("""[a-zA-Z][a-zA-Z0-9_]*""".r, "illegal entry name")

    val treeKey: Parser[String] = '<' ~> ws ~> """[a-zA-Z0-9_.]*""".r <~ ws <~ '>'

    val mapKey: Parser[String] = '(' ~> ws ~> """[^)]*""".r <~ ws <~ ')'

    val entryId: Parser[EntryId] =
      entryName ~ treeKey ^^ { case name ~ key => TreeEntryId(name, key) } |
      entryName ~ mapKey ^^ { case name ~ key => MapEntryId(name, key) } |
      entryName ^^ { case name => SimpleEntryId(name) }

    /**
     * 
     */
    val messageFormat: Parser[MessageFormat] = namedError(
      rep(
        """\\""" ^^ (_ => """\""")                          | // Handle escaped \\
        """\""" ~> ws ~> ("\r"?) ~> "\n" ~> ws ^^ (_ => "") | // Ignore escaped end of lines and following whitespace
        """\n""" ^^ (_ => "\n")                             | // Translate literal \n to real newline
        """.""".r                                             // keep any character (except new line)
      ) ^^ { case frags => new MessageFormat(frags.mkString.trim) },
      "message format expected"
    )

    val entryValue: Parser[EntryValue] =
      "->" ~> ws ~> entryName ^^ { case n => LinkEntryValue(n) } |
      '@' ~> ws ~> messageFormat ^^ { case m => MsgEntryValue(m, true) } |
      '=' ~> ws ~> messageFormat ^^ { case m => MsgEntryValue(m, false) }

    val entry: Parser[EntryLine] = (ws ~> entryId) ~ (ws ~> entryValue) ^^ {
      case k ~ v => EntryLine(k, v)
    }

    val comment: Parser[Option[EntryLine]] = """#.*""".r ^^ { case s => None }

    val line: Parser[Option[EntryLine]] = entry ^^ { case e => Some(e) } | comment | ws ^^ { _ => None }

    val lines: Parser[List[EntryLine]] = repsep(line, newLine) ^^ { case ls => ls collect { case Some(re) => re } }

    def namedError[A](p: Parser[A], msg: String) = Parser[A] { i =>
      p(i) match {
        case Failure(_, in) => Failure(msg, in)
        case o => o
      }
    }

    val phraseLines: Parser[List[EntryLine]] = phrase(lines)

    def parse(url: URL): List[EntryLine] = {
      val input = StreamReader(Source.fromURL(url, "UTF-8").reader())
      phraseLines(input) match {
        case Success(lines, _) => lines
        case NoSuccess(message, in) => throw new Exception(s"parse error - message: $message; line: ${in.pos.line}; column: ${in.pos.column}; url: ${url}")
      }
    }

  }


}