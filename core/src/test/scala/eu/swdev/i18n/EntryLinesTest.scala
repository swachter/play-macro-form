package eu.swdev.i18n

import org.scalatest.{Inside, FunSuite}
import scala.util.parsing.input.CharSequenceReader
import scala.language.implicitConversions

/**
  */
class EntryLinesTest extends FunSuite with Inside {

  import EntryLines.ResourceParser

  implicit def toReader(string: String) = new CharSequenceReader(string)

  def parseEntry(string: String): ResourceParser.ParseResult[EntryLine] = {
    ResourceParser.phrase(ResourceParser.entry)(string)
  }

  def parseLines(string: String): ResourceParser.ParseResult[List[EntryLine]] = {
    ResourceParser.phraseLines(string)
  }

  test("ResourceEntry") {
    inside(parseEntry("a=")) {
      case ResourceParser.Success(EntryLine(SimpleEntryLineId("a"), MsgEntryLineValue(mf, false)), _) if (mf.toPattern == "") =>
    }
    inside(parseEntry("a=b")) {
      case ResourceParser.Success(EntryLine(SimpleEntryLineId("a"), MsgEntryLineValue(mf, false)), _) if (mf.toPattern == "b") =>
    }
    inside(parseEntry("a@b")) {
      case ResourceParser.Success(EntryLine(SimpleEntryLineId("a"), MsgEntryLineValue(mf, true)), _) if (mf.toPattern == "b") =>
    }
    inside(parseEntry("a=b\\\nc")) {
      case ResourceParser.Success(EntryLine(_, MsgEntryLineValue(mf, _)), _) if (mf.toPattern == "bc") =>
    }
    inside(parseEntry("a=b\\nc")) {
      case ResourceParser.Success(EntryLine(_, MsgEntryLineValue(mf, _)), _) if (mf.toPattern == "b\nc") =>
    }
  }

  test("List[ResourceEntry]") {
    inside(parseLines("")) {
      case ResourceParser.Success(List(), _) =>
    }
    inside(parseLines("a=b")) {
      case ResourceParser.Success(List(EntryLine(_, _)), _) =>
    }
    inside(parseLines("\na=b\n")) {
      case ResourceParser.Success(List(EntryLine(_, _)), _) =>
    }
    inside(parseLines("  \na=b\n  ")) {
      case ResourceParser.Success(List(EntryLine(_, _)), _) =>
    }
    inside(parseLines("#cmt\na=b\n#cmt")) {
      case ResourceParser.Success(List(EntryLine(_, _)), _) =>
    }
    inside(parseLines("#cmt\na=b\n#cmt\n")) {
      case ResourceParser.Success(List(EntryLine(_, _)), _) =>
    }
    inside(parseLines("a=b\na@b")) {
      case ResourceParser.Success(List(EntryLine(_, _), EntryLine(_, _)), _) =>
    }
  }

}
