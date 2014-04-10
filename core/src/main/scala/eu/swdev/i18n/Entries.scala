package eu.swdev.i18n

import java.text.MessageFormat
import eu.swdev.config.StringKeyValueTreeModule

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
  def emptyEntry = new TreeEntry(this, TreeEntry.emptyTree)
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

case class TreeEntry(tpe: EntryType, tree: TreeEntry.Tree) extends LookupEntry {

  override def lookup(key: String): Option[Entry] = tree.getValue(key)
  def +(key: String, value: Entry): TreeEntry = copy(tree = tree(key) = value)
}

object TreeEntry {
  object TreeModule extends StringKeyValueTreeModule {
    type Value = Entry
  }
  type Tree = TreeModule.KeyValueTree
  def emptyTree: Tree = TreeModule.KeyValueTree.empty
}

case class MapEntry(tpe: EntryType, map: Map[String, Entry]) extends LookupEntry {

  override def lookup(key: String): Option[Entry] = map.get(key)
  def +(key: String, value: Entry): MapEntry = copy(map = map + (key -> value))
}
