package eu.swdev.i18n

import java.text.MessageFormat
import scala.io.Source
import scala.util.parsing.combinator.RegexParsers
import java.net.URL
import scala.util.parsing.input.StreamReader

import scala.language.postfixOps
import scala.collection.GenTraversable


/**
 * Represent a key for an entry in a resource file.
 *
 * The key represents everything that is left to the assignment operator in a resource entry line.
 */
sealed trait EntryKey {
  /**
   * The name of a resource entry. In case of tree-valued or map-valued resource entries the same name is shared by
   * all resource entries belonging to the same tree or map.
   *
   * @return
   */
  def name: EntryName
}

/**
 *
 * @param name identifies a message format
 */
case class SimpleEntryKey(name: EntryName) extends EntryKey

/**
 * Represents a key for a tree-valued resource entry.
 *
 * There might be several tree entries with the same name but different paths. All tree entries with the same name
 * are used together to define a single key-value tree (cf. [[eu.swdev.config.KeyValueTreeModule]]). That tree in turn
 * can later be used to lookup messages using a hierarchical criterion (e.g. a path).
 *
 * @param name identifies a key-value tree of message formats
 * @param path addresses a part of the key-value tree
 */
case class TreeEntryKey(name: EntryName, path: String) extends EntryKey

//case class MapEntryKey(name: EntryName, key: String) extends EntryKey

sealed trait EntryValue

case class MsgEntryValue(format: MessageFormat, isMarkup: Boolean) extends EntryValue

case class LinkEntryValue(name: EntryName) extends EntryValue

/**
 * Represents an entry in a resource file.
 * 
 * An entry typically consists of a single line
 *
 * @param key the key of the entry;
 * @param value
 */
case class Entry(key: EntryKey, value: EntryValue)

sealed trait ResType {
  def args: Int
  def isMarkup: Boolean
}

/**
 * Describes an entry that corresponds to one or more message formats.
 * 
 * @param args the maximum number of arguments a message has
 * @param isMarkup indicates if the message contains markup or not
 */
case class MsgResType(args: Int, isMarkup: Boolean) extends ResType

case class TreeResType(nested: ResType) extends ResType {
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
  def asMsg: MsgResValue
  def asTree: TreeResValue
}

/**
 *
 * @param format
 * @param isMarkup indicates if the format contains markup or not
 */
case class MsgResValue(format: MessageFormat, isMarkup: Boolean) extends ResValue {
  override def asTree: TreeResValue = throw new UnsupportedOperationException
  override def asMsg: MsgResValue = this

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
  def markupMsg(args: Array[Object])(implicit markup: MsgMarkup): markup.M = {
    val s = format.format(args)
    if (isMarkup) markup.markupMsg(s) else markup.rawMsg(s)
  }

}

case class TreeResValue(tree: ResTrees.KeyValueTree) extends ResValue {
  def getValue(path: String): Option[ResValue] = tree.getValue(path)
  override def asTree: TreeResValue = this
  override def asMsg: MsgResValue = throw new UnsupportedOperationException
}

/**
 *
 * @param entries The sequential arrangement of the entries correspond to their appearance in definitions. Entries that
 *                were defined earlier are in front of entries that were defined later. If an entry is overwritten
 *                by a later definition then its earlier appearance is removed.
 */
case class ResourceEntries private (entries: List[Entry]) {

  def add(other: ResourceEntries): ResourceEntries = this ++ other.entries

  def ++(other: GenTraversable[Entry]): ResourceEntries = {
    val zero = (List.empty[Entry], Set.empty[EntryKey])
    val t = (entries ++ other).foldRight(zero)((e, accu) => if (accu._2.contains(e.key)) accu else (e :: accu._1, accu._2 + e.key))
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
      println(s"parsing url: $url")
      ResourceParser.parse(url)
    }
    empty ++ lines.toList.flatten
  }

  object ResourceParser extends RegexParsers {

    override def skipWhitespace = false
    override val whiteSpace = """[ \t]+""".r

    def ignoreWhiteSpace = opt(whiteSpace)
    def newLine = namedError((("\r"?) ~> "\n"), "End of line expected")

    val keyName: Parser[String] = namedError("""[a-zA-Z][a-zA-Z0-9_]*""".r, "illegal message key expected")

    val keyPath: Parser[String] = '[' ~> """[a-zA-Z0-9_.]*""".r <~ ']'

    val entryKey: Parser[EntryKey] = (keyName ~ opt(keyPath)) ^^ {
      case id ~ None => SimpleEntryKey(id)
      case id ~ Some(path) => TreeEntryKey(id, path)
    }

    val messageFormatValue: Parser[MessageFormat] = namedError(
      rep(
        """\""" ~> ("\r"?) ~> "\n" ^^ (_ => "") | // Ignore escaped end of lines \
          """\n""" ^^ (_ => "\n") | // Translate literal \n to real newline
          """\\""" ^^ (_ => """\""") | // Handle escaped \\
          """.""".r // Or any character (except new line)
      ) ^^ { case frags => new MessageFormat(frags.mkString.trim) },
      "message format expected"
    )

    val entryValue: Parser[EntryValue] =
      ("->" ~> ignoreWhiteSpace ~> keyName ^^ { case id => LinkEntryValue(id) }) |
      ('@' ~> ignoreWhiteSpace ~> messageFormatValue ^^ { case v => MsgEntryValue(v, true) }) |
      ('=' ~> ignoreWhiteSpace ~> messageFormatValue ^^ { case v => MsgEntryValue(v, false) })

    val entry: Parser[Entry] = (ignoreWhiteSpace ~> entryKey) ~ (ignoreWhiteSpace ~> entryValue) ^^ {
      case k ~ v => Entry(k, v)
    }

    val comment: Parser[Option[Entry]] = """#.*""".r ^^ { case s => None }

    val line: Parser[Option[Entry]] = entry ^^ { case e => Some(e) } | comment | ignoreWhiteSpace ^^ { _ => None }

    val lines: Parser[List[Entry]] = repsep(line, newLine) ^^ { case ls => ls collect { case Some(re) => re } }

    def namedError[A](p: Parser[A], msg: String) = Parser[A] { i =>
      p(i) match {
        case Failure(_, in) => Failure(msg, in)
        case o => o
      }
    }

    val phraseLines: Parser[List[Entry]] = phrase(lines)

    def parse(url: URL): List[Entry] = {
      val input = StreamReader(Source.fromURL(url, "UTF-8").reader())
      phraseLines(input) match {
        case Success(lines, _) => lines
        case NoSuccess(message, in) => throw new Exception(s"parse error - message: $message; line: ${in.pos.line}; column: ${in.pos.column}; url: ${url}")
      }
    }

  }


}