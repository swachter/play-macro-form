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
      case AnalyzeResult(MapE(("a", MsgResType(0, false)), ("b", MsgResType(1, true)), ("c", MsgResType(2, false)), ("d", MsgResType(3, false)), ("o", MsgResType(0, false)), ("t", TreeResType(MsgResType(3, false)))), _, _, _) => true
    }
  }

  def parseLines(string: String): List[Entry] = {
    ResourceEntries.ResourceParser.phraseLines(new CharSequenceReader(string)).get
  }


  test("tree of trees") {
    val input =
      """
        |a[1]=a1
        |a[2]=a2
        |b[1]=b1
        |b[2]=b2
        |u[1]->a
        |u[2]->b
      """.stripMargin

    val entries: List[Entry] = parseLines(input)

    val (names, _) = orderEntries(entries)

    val result = determineEntryTypesForOneLocale(entries, names)

    inside(result) {
      case MapE(("a", List(TreeResType(MsgResType(0, false)))), ("b", List(TreeResType(MsgResType(0, false)))), ("u", List(TreeResType(TreeResType(MsgResType(0, false)))))) => true
    }
  }
}
