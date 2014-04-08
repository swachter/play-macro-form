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
  def id: String
}

/**
 *
 * @param id identifies a message format
 */
case class SimpleEntryKey(id: String) extends EntryKey

/**
 * Represents a key for a lookup entry.
 *
 * There might be several lookup entries with the same id but different paths. All lookup entries with the same id
 * are used together to define a single key-value tree (cf. [[eu.swdev.config.KeyValueTreeModule]]). That tree in turn
 * can later be used to lookup messages using a hierarchical criterion (e.g. a path).
 *
 * @param id identifies a key-value tree of message formats
 * @param path addresses a part of the key-value tree
 */
case class LookupEntryKey(id: String, path: String) extends EntryKey

/**
 * Represent a resource entry in a resource file.
 *
 * @param key the key of the entry;
 * @param msg
 * @param isMarkup
 */
case class ResourceEntry(key: EntryKey, msg: MessageFormat, isMarkup: Boolean)

/**
 *
 * @param entries The sequential arrangement of the entries correspond to their appearance in definitions. Entries that
 *                were defined earlier are in front of entries that were defined later. If an entry is overwritten
 *                by a later definition then its earlier appearance is removed.
 */
case class ResourceEntries private (entries: List[ResourceEntry]) {

  def add(other: ResourceEntries): ResourceEntries = this ++ other.entries

  def ++(other: GenTraversable[ResourceEntry]): ResourceEntries = {
    val zero = (List.empty[ResourceEntry], Set.empty[EntryKey])
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
      ResourceParser.parse(url)
    }
    empty ++ lines.toList.flatten
  }

  object ResourceParser extends RegexParsers {

    override def skipWhitespace = false
    override val whiteSpace = """[ \t]+""".r

    def ignoreWhiteSpace = opt(whiteSpace)
    def newLine = namedError((("\r"?) ~> "\n"), "End of line expected")

    val keyId: Parser[String] = namedError("""[a-zA-Z][a-zA-Z0-9_]*""".r, "illegal message key expected")

    val keyPath: Parser[String] = '[' ~> """[a-zA-Z0-9_.]*""".r <~ ']'

    val key: Parser[EntryKey] = (keyId ~ opt(keyPath)) ^^ {
      case id ~ None => SimpleEntryKey(id)
      case id ~ Some(path) => LookupEntryKey(id, path)
    }

    val assignOp: Parser[Boolean] = '@' ^^ { case _ => true } | '=' ^^ { case _ => false }

    def value: Parser[MessageFormat] = namedError(
      rep(
        """\""" ~> ("\r"?) ~> "\n" ^^ (_ => "") | // Ignore escaped end of lines \
          """\n""" ^^ (_ => "\n") | // Translate literal \n to real newline
          """\\""" ^^ (_ => """\""") | // Handle escaped \\
          """.""".r // Or any character (except new line)
      ) ^^ { case frags => new MessageFormat(frags.mkString.trim) },
      "message format expected"
    )

    val entry: Parser[ResourceEntry] = (ignoreWhiteSpace ~> key) ~ (ignoreWhiteSpace ~> assignOp) ~ (ignoreWhiteSpace ~> value) ^^ {
      case k ~ op ~ v => ResourceEntry(k, v, op)
    }

    val comment: Parser[Option[ResourceEntry]] = """#.*""".r ^^ { case s => None }

    val line: Parser[Option[ResourceEntry]] = entry ^^ { case e => Some(e) } | comment | ignoreWhiteSpace ^^ { _ => None }

    val lines: Parser[List[ResourceEntry]] = repsep(line, newLine) ^^ { case ls => ls collect { case Some(re) => re } }

    def namedError[A](p: Parser[A], msg: String) = Parser[A] { i =>
      p(i) match {
        case Failure(_, in) => Failure(msg, in)
        case o => o
      }
    }

    val phraseLines: Parser[List[ResourceEntry]] = phrase(lines)

    def parse(url: URL): List[ResourceEntry] = {
      val input = StreamReader(Source.fromURL(url, "UTF-8").reader())
      phraseLines(input) match {
        case Success(lines, _) => lines
        case NoSuccess(message, in) => throw new Exception(s"parse error - message: $message; line: ${in.pos.line}; column: ${in.pos.column}; url: ${url}")
      }
    }

  }


}