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
sealed trait EntryLineId {
  /**
   * The name of an entry. In case of tree-valued or map-valued entries the same entry name appears on several entry lines.
   * In that case all the lines belonging to the same entry are used together to build the corresponding tree or map.
   *
   * @return
   */
  def name: EntryName
}

sealed trait LookupEntryLineId extends EntryLineId {
  /**
   * A lookup key.
   * @return
   */
  def key: String
}

/**
 * Identifies an entry line that defines an entry that is either a message entry or a reference to another entry.
 *
 * @param name
 */
case class SimpleEntryLineId(name: EntryName) extends EntryLineId

/**
 * Identifies an entry line belonging to a tree-valued entry.
 *
 * @param name the name of the entry
 * @param key a dot separated string that represents a path in the tree; the entry line defines the value for this path
 */
case class TreeEntryLineId(name: EntryName, key: String) extends LookupEntryLineId

/**
 * Identifies an entry line belonging to a map-valued entry.
 *
 * @param name the name of the entry
 * @param key the key for which this entry line defines a value
 */
case class MapEntryLineId(name: EntryName, key: String) extends LookupEntryLineId

object LookupEntryLineId {
  def unapplySeq(lineId: LookupEntryLineId): Option[Seq[String]] = Some(Seq(lineId.name, lineId.key))
}

/**
 * Represent the value in an entry line.
 */
sealed trait EntryLineValue

/**
 * Represents an entry line value that is a message.
 * 
 * @param format the message format that is used to generate the message
 * @param isMarkup indicates if the message contains markup or not
 */
case class MsgEntryLineValue(format: MessageFormat, isMarkup: Boolean) extends EntryLineValue

/**
 * Represents a message line value that is a link to another entry.
 *
 * @param name
 */
case class LinkEntryLineValue(name: EntryName) extends EntryLineValue

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
case class EntryLine(id: EntryLineId, value: EntryLineValue)

/**
 * Describes the type of an entry.
 *
 * One or message formats lie behind an entry. For example an entry may comprise message formats for different locales
 * or in case of tree-valued or map-valued entries for different keys. An entry type provides an aggregated view of all
 * the corresponding message formats. 
 */
sealed trait EntryType {
  /**
   * Gets the maximum number of arguments of the message formats behind this entry type.
   * @return
   */
  def args: Int
  /**
   * Indicates if any of the message formats behind this entry type consist of markup.
   * @return
   */
  def isMarkup: Boolean
}

sealed trait LookupEntryType extends EntryType {
  protected def nested: EntryType
  def args: Int = nested.args
  def isMarkup: Boolean = nested.isMarkup
  def emptyEntry: LookupEntry
}

/**
 * Describes an entry that corresponds to one or more message formats.
 * 
 * @param args the maximum number of arguments a message has
 * @param isMarkup indicates if the message contains markup or not
 */
case class MsgEntryType(args: Int, isMarkup: Boolean) extends EntryType

case class TreeEntryType(nested: EntryType) extends LookupEntryType {
  def emptyEntry = new TreeEntry(this, ResTrees.KeyValueTree.empty)
}

case class MapEntryType(nested: EntryType) extends LookupEntryType {
  def emptyEntry = new MapEntry(this, Map.empty)
}

object MsgEntryType {
  def apply(format: MessageFormat, isMarkup: Boolean): MsgEntryType = MsgEntryType(format.getFormatsByArgumentIndex.length, isMarkup)
}

//
//
//

/**
 * Represents a resource entry.
 */
sealed trait Entry {

  def tpe: EntryType

  /**
   * Format the message and return it as raw string.
   *
   * This method must be called only if this entry is a message entry.
   *
   * @param args
   * @return
   */
  def outputRaw(args: Array[Object]): String

  /**
   * Format the message and return it as markup.
   *
   * This method must be called only if this entry is a message entry.
   *
   * @param args
   * @param markup
   * @return
   */
  def outputMarkup(args: Array[Object])(implicit markup: MsgMarkup): markup.M

  /**
   * Lookup an entry.
   *
   * This method must be called only if this entry is either map-valued or tree-valued.
   *
   * @param key
   * @return
   */
  def lookup(key: String): Option[Entry]

}

trait LookupEntry extends Entry {
  override def outputRaw(args: Array[Object]): String = throw new UnsupportedOperationException
  override def outputMarkup(args: Array[Object])(implicit markup: MsgMarkup): markup.M = throw new UnsupportedOperationException
  def +(key: String, value: Entry): LookupEntry
}

/**
 *
 * @param format
 * @param isMarkup indicates if the format contains markup or not
 */
case class MsgEntry(tpe: EntryType, format: MessageFormat, isMarkup: Boolean) extends Entry {

  override def lookup(key: String): Option[Entry] = throw new UnsupportedOperationException

  override def outputRaw(args: Array[Object]): String = {
    format.format(args)
  }
  override def outputMarkup(args: Array[Object])(implicit markup: MsgMarkup): markup.M = {
    val s = format.format(args)
    if (isMarkup) markup.markupMsg(s) else markup.rawMsg(s)
  }
}

case class TreeEntry(tpe: EntryType, tree: ResTrees.KeyValueTree) extends LookupEntry {

  override def lookup(key: String): Option[Entry] = tree.getValue(key)
  def +(key: String, value: Entry): TreeEntry = copy(tree = tree(key) = value)
}

case class MapEntry(tpe: EntryType, map: Map[String, Entry]) extends LookupEntry {

  override def lookup(key: String): Option[Entry] = map.get(key)
  def +(key: String, value: Entry): MapEntry = copy(map = map + (key -> value))
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
    val zero = (List.empty[EntryLine], Set.empty[EntryLineId])
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

    val entryId: Parser[EntryLineId] =
      entryName ~ treeKey ^^ { case name ~ key => TreeEntryLineId(name, key) } |
      entryName ~ mapKey ^^ { case name ~ key => MapEntryLineId(name, key) } |
      entryName ^^ { case name => SimpleEntryLineId(name) }

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

    val entryValue: Parser[EntryLineValue] =
      "->" ~> ws ~> entryName ^^ { case n => LinkEntryLineValue(n) } |
      '@' ~> ws ~> messageFormat ^^ { case m => MsgEntryLineValue(m, true) } |
      '=' ~> ws ~> messageFormat ^^ { case m => MsgEntryLineValue(m, false) }

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