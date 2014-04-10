package eu.swdev.i18n

import org.scalatest.{Inside, FunSuite}
import scala.util.parsing.input.CharSequenceReader
import scala.language.implicitConversions

/**
  */
class EntryLinesTest extends FunSuite with Inside {

  implicit def toReader(string: String) = new CharSequenceReader(string)

  def parseEntry(string: String): Either[(String, EntryLinesParser.Input, String), EntryLine] = {
    EntryLinesParser.parse(string, EntryLinesParser.phrase(EntryLinesParser.entry))
  }

  def parseLines(string: String): Either[(String, EntryLinesParser.Input, String), List[EntryLine]] = {
    EntryLinesParser.parsePhraseLines(string)
  }

  test("ResourceEntry") {
    inside(parseEntry("a=")) {
      case Right(EntryLine(SimpleEntryLineId("a"), MsgEntryLineValue(mf, false))) if (mf.toPattern == "") =>
    }
    inside(parseEntry("a=b")) {
      case Right(EntryLine(SimpleEntryLineId("a"), MsgEntryLineValue(mf, false))) if (mf.toPattern == "b") =>
    }
    inside(parseEntry("a@b")) {
      case Right(EntryLine(SimpleEntryLineId("a"), MsgEntryLineValue(mf, true))) if (mf.toPattern == "b") =>
    }
    inside(parseEntry("a=b\\\nc")) {
      case Right(EntryLine(_, MsgEntryLineValue(mf, _))) if (mf.toPattern == "bc") =>
    }
    inside(parseEntry("a=b\\nc")) {
      case Right(EntryLine(_, MsgEntryLineValue(mf, _))) if (mf.toPattern == "b\nc") =>
    }
  }

  test("List[ResourceEntry]") {
    inside(parseLines("")) {
      case Right(List()) =>
    }
    inside(parseLines("a=b")) {
      case Right(List(EntryLine(_, _))) =>
    }
    inside(parseLines("\na=b\n")) {
      case Right(List(EntryLine(_, _))) =>
    }
    inside(parseLines("  \na=b\n  ")) {
      case Right(List(EntryLine(_, _))) =>
    }
    inside(parseLines("#cmt\na=b\n#cmt")) {
      case Right(List(EntryLine(_, _))) =>
    }
    inside(parseLines("#cmt\na=b\n#cmt\n")) {
      case Right(List(EntryLine(_, _))) =>
    }
    inside(parseLines("a=b\na@b")) {
      case Right(List(EntryLine(_, _), EntryLine(_, _))) =>
    }
  }

}
