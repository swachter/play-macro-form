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
      case AnalyzeResult(SetE("a", "b", "c", "d", "o"), SetE("t"), MapE(("a", MsgSignature(0, false)), ("b", MsgSignature(1, true)), ("c", MsgSignature(2, false)), ("d", MsgSignature(3, false)), ("o", MsgSignature(0, false)), ("t", MsgSignature(3, false))), MapE((de_DE, SetE())), SetE()) => true
    }
  }
}
