package eu.swdev.i18n

import org.scalatest.{Inside, FunSuite}
import scala.util.parsing.input.CharSequenceReader

/**
  */
class ResourceEntriesTest extends FunSuite with Inside {

  import ResourceEntries.ResourceParser

  implicit def toReader(string: String) = new CharSequenceReader(string)

  def parseEntry(string: String): ResourceParser.ParseResult[EntryLine] = {
    ResourceParser.phrase(ResourceParser.entry)(string)
  }

  def parseLines(string: String): ResourceParser.ParseResult[List[EntryLine]] = {
    ResourceParser.phraseLines(string)
  }

  test("ResourceEntry") {
    inside(parseEntry("a=")) {
      case ResourceParser.Success(EntryLine(SimpleEntryId("a"), MsgEntryValue(mf, false)), _) if (mf.toPattern == "") => true
    }
    inside(parseEntry("a=b")) {
      case ResourceParser.Success(EntryLine(SimpleEntryId("a"), MsgEntryValue(mf, false)), _) if (mf.toPattern == "b") => true
    }
    inside(parseEntry("a@b")) {
      case ResourceParser.Success(EntryLine(SimpleEntryId("a"), MsgEntryValue(mf, true)), _) if (mf.toPattern == "b") => true
    }
    inside(parseEntry("a=b\\\nc")) {
      case ResourceParser.Success(EntryLine(_, MsgEntryValue(mf, _)), _) if (mf.toPattern == "bc") => true
    }
    inside(parseEntry("a=b\\nc")) {
      case ResourceParser.Success(EntryLine(_, MsgEntryValue(mf, _)), _) if (mf.toPattern == "b\nc") => true
    }
  }

  test("List[ResourceEntry]") {
    inside(parseLines("")) {
      case ResourceParser.Success(List(), _) => true
    }
    inside(parseLines("a=b")) {
      case ResourceParser.Success(List(EntryLine(_, _)), _) => true
    }
    inside(parseLines("\na=b\n")) {
      case ResourceParser.Success(List(EntryLine(_, _)), _) => true
    }
    inside(parseLines("  \na=b\n  ")) {
      case ResourceParser.Success(List(EntryLine(_, _)), _) => true
    }
    inside(parseLines("#cmt\na=b\n#cmt")) {
      case ResourceParser.Success(List(EntryLine(_, _)), _) => true
    }
    inside(parseLines("#cmt\na=b\n#cmt\n")) {
      case ResourceParser.Success(List(EntryLine(_, _)), _) => true
    }
    inside(parseLines("a=b\na@b")) {
      case ResourceParser.Success(List(EntryLine(_, _), EntryLine(_, _)), _) => true
    }
  }

}
