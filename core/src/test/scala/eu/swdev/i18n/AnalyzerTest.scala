package eu.swdev.i18n

import org.scalatest.{Inside, FunSuite}
import java.util.Locale
import scala.util.parsing.input.CharSequenceReader


/**
  */
class AnalyzerTest extends FunSuite with Inside {

  import Analyzer._

  val de_DE = new Locale("de", "DE")

  object SetE {
    def unapplySeq(s: Set[String]): Option[Seq[String]] = Some(s.toSeq.sorted)
  }

  object MapE {
    def unapplySeq[K:Ordering, V](m: Map[K, V]): Option[Seq[(K, V)]] = Some(m.toSeq.sortWith((kv1, kv2) => implicitly[Ordering[K]].compare(kv1._1, kv2._1) < 0))
  }

  implicit val localeOrdering = Ordering.by[Locale, String](l => l.toString)

  test("simple") {
    val result = analyze(getClass.getClassLoader, "com/abc/resource", de_DE)
    inside(result) {
      case AnalyzeResult(MapE(("a", MsgEntryType(0, false)), ("b", MsgEntryType(1, true)), ("c", MsgEntryType(2, false)), ("d", MsgEntryType(3, false)), ("o", MsgEntryType(0, false)), ("t", TreeEntryType(MsgEntryType(3, false)))), _, _, _) =>
    }
  }

  def parseLines(string: String): List[EntryLine] = {
    EntryLinesParser.parsePhraseLines(string).right.get
  }


  test("tree of trees") {
    val input =
      """
        |a<1>=a1
        |a<2>=a2
        |b<1>=b1
        |b<2>=b2
        |u<1>->a
        |u<2>->b
      """.stripMargin

    val entries: List[EntryLine] = parseLines(input)

    val (names, _) = orderEntries(entries)

    val result = determineEntryTypesForOneLocale(entries, names)

    inside(result) {
      case MapE(("a", List(TreeEntryType(MsgEntryType(0, false)))), ("b", List(TreeEntryType(MsgEntryType(0, false)))), ("u", List(TreeEntryType(TreeEntryType(MsgEntryType(0, false)))))) =>
    }
  }
}
