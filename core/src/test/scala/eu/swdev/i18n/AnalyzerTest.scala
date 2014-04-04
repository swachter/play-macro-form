package eu.swdev.i18n

import org.scalatest.{Inside, FunSuite}
import java.util.Locale


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
    val s = Set("a")
    inside(result) {
      case AnalyzeResult(SetE("a", "b", "c", "d", "o"), SetE("t"), MapE(("a", 0), ("b", 1), ("c", 2), ("d", 3), ("o", 0), ("t", 3)), MapE((de_DE, SetE())), SetE()) => true
    }
  }
}
