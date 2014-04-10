package eu.swdev.i18n

import java.text.MessageFormat
import scala.collection.GenTraversable
import java.net.URL
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.StreamReader
import scala.io.Source
import scala.language.postfixOps

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
 * Contains a list of entry lines.
 *
 * The sequential order in the list of the entry lines correspond to their appearance in definitions. Entry lines that
 * appeared earlier are in front of entry lines that appeared later. If an entry line that appeared later has the same
 * entry line id as a preceding entry line then the preceding entry line is overwritten (i.e. it is no more contained
 * in the list of entry lines).
 */
case class EntryLines private (entries: List[EntryLine]) {

  def add(other: EntryLines): EntryLines = this ++ other.entries

  def ++(other: GenTraversable[EntryLine]): EntryLines = {
    val zero = (List.empty[EntryLine], Set.empty[EntryLineId])
    val t = (entries ++ other).foldRight(zero)((e, accu) => if (accu._2.contains(e.id)) accu else (e :: accu._1, accu._2 + e.id))
    EntryLines(t._1)
  }
}

object EntryLines {

  val empty = new EntryLines(Nil)

  def apply(url: URL): EntryLines = {
    val lines = parse(url)
    empty ++ lines
  }

  def apply(urls: TraversableOnce[URL]): EntryLines = {
    val liness = for {
      url <- urls
    } yield {
      parse(url)
    }
    empty ++ liness.toList.flatten
  }

  private def parse(url: URL): List[EntryLine] = {
    EntryLinesParser.parsePhraseLines(url) match {
      case Left(l) => throw new Exception(s"parse error - message: ${l._1}; line: ${l._2.pos.line}; column: ${l._2.pos.column}; url: ${l._3}")
      case Right(r) => r
    }
  }
}