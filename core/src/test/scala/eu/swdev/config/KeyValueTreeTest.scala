package eu.swdev.config

import org.scalatest.FunSuite
import org.scalacheck.Prop.forAll
import org.scalacheck.Gen
import scala.util.matching.Regex
import org.scalatest.prop.Checkers

/**
 */
class KeyValueTreeTest extends FunSuite with Checkers {

  val kvtm = new StringKeyValueTreeModule {
    type Value = String
  }

  test("simple") {
    val t = kvtm.KeyValueTree(Seq(("*.b", "x")))

    assert(t.getValue("a.b") == Some("x"))
  }

  val nonWildcardGen: Gen[String] = for {
    c <- Gen.choose('a', 'b')
  } yield {
    c.toString
  }

  val defStepGen: Gen[String] = Gen.frequency((1, "*"), (1, "?"), (2, nonWildcardGen))

  val defKeyGen: Gen[List[String]] = Gen.choose(1, 4).flatMap(Gen.listOfN(_, defStepGen))

  val defKeysGen: Gen[List[List[String]]] = Gen.choose(1, 20).flatMap(Gen.listOfN(_, defKeyGen))

  val keyGen: Gen[List[String]] = Gen.choose(1, 4).flatMap(Gen.listOfN(_, nonWildcardGen))

  test("split") {
    assert(kvtm.splitKey("a.b.c") === List("a", "b", "c"))
    assert(kvtm.splitKey("a..c") === List("a", "", "c"))
    assert(kvtm.splitKey(".b.c") === List("", "b", "c"))
    assert(kvtm.splitKey("a.b.") === List("a", "b", ""))
  }

  test("nonWildcardGen") {
    check(forAll(nonWildcardGen) { str =>
      str.length == 1
    })
  }

//  test("keyGen") {
//    check(forAll(keyGen) { key =>
//      key.length > 1 && key.forall(_.length == 1)
//    })
//  }

  private def mkRegex(steps: List[String]): Regex = {
    steps.foldLeft(List.empty[RegExStep]){(list, step) =>
      if (kvtm.isAsteriskStep(step)) {
        list match {
          case WildcardRegExStep(minSteps, _, hasPredecessor, hasSuccessor) :: t => WildcardRegExStep(minSteps, true, hasPredecessor, hasSuccessor) :: t
          case _ => WildcardRegExStep(0, true, !list.isEmpty, false) :: list
        }
      } else if (kvtm.isQuestionMarkStep(step)) {
        list match {
          case WildcardRegExStep(minSteps, isUnbounded, hasPredecessor, hasSuccessor) :: t => WildcardRegExStep(minSteps + 1, isUnbounded, hasPredecessor, hasSuccessor) :: t
          case _ => WildcardRegExStep(1, false, !list.isEmpty, false) :: list
        }
      } else {
        list match {
          case NonWildcardRegExStep(steps) :: t => NonWildcardRegExStep(steps :+ step) :: t
          case WildcardRegExStep(minSteps, isUnbounded, hasPredecessor, _) :: t => NonWildcardRegExStep(Seq(step)) :: WildcardRegExStep(minSteps, isUnbounded, hasPredecessor, true) :: t
          case _ => NonWildcardRegExStep(Seq(step)) :: list
        }

      }
    }.reverse.map(_.regExFrag).mkString("").r.anchored
  }

  sealed trait RegExStep {
    def regExFrag: String
  }

  case class WildcardRegExStep(minSteps: Int, isUnbounded: Boolean, hasPredecessor: Boolean, hasSuccessor: Boolean) extends RegExStep {
    def regExFrag: String = {
      if (minSteps > 0) {
        val p = if (hasPredecessor) "\\." else ""
        val s = if (hasSuccessor) "\\." else ""
        val u = if (isUnbounded) "(\\.[^.]*)*"
        List.fill(minSteps)("[^.]*").mkString(p, "\\.", u + s)
      } else {
        // it is unbounded with minSteps == 0
        if (hasPredecessor && hasSuccessor) {
          "\\.(.*\\.)?"
        } else if (hasPredecessor) {
          "(\\..*)?"
        } else if (hasSuccessor) {
          "(.*\\.)?"
        } else {
          ".*"
        }
      }
    }
  }

  case class NonWildcardRegExStep(steps: Seq[String]) extends RegExStep {
    def regExFrag = steps.mkString("\\.")
  }

  test("a.*.?") {
    val r = mkRegex(List("a", "*", "?"))
    println(s"r: $r")
    assert(r.pattern.matcher("a.a.b").matches())
  }

  test("*.?.*") {
    val tree = kvtm.KeyValueTree(Seq(("*.?.*", "x")))
    assert(tree.getValue("a") === Some("x"))
  }

  test("? - b.a.b") {
    val tree = kvtm.KeyValueTree(Seq(("?", "x")))
    assert(tree.getValue("b.a.b") === None)
  }

  test("scala-check") {

    check(forAll(defKeysGen) { defKeys =>
      val defKeysInfo: List[(String, Regex, Int)] = defKeys.map(k =>
        (
          k.mkString("."),
          mkRegex(k),
          k.map(s =>
            if (kvtm.isAsteriskStep(s)) 0
            else if (kvtm.isQuestionMarkStep(s)) 1
            else 10
          ).sum
        )
      )
      def failure(msg: String): Boolean = {
        println(msg)
        defKeysInfo.foreach(i => println(s"defKey: ${i._1}"))
        false
      }
      val tree = kvtm.KeyValueTree(defKeysInfo.map(i => (i._1, i._1)))
      forAll(keyGen) { key =>
        val strKey = key.mkString(".")
        val value: Option[String] = tree.getValue(strKey)
        val regExMatches = defKeysInfo.filter(_._2.pattern.matcher(strKey).matches())
        if (regExMatches.isEmpty && value.isEmpty) {
          println(s"ok: $strKey -> None")
          true
        } else if (regExMatches.isEmpty) {
          failure(s"regex matches is empty - strKey: $strKey; value: $value")
        } else if (value.isEmpty) {
          failure(s"value is empty - strKey: $strKey; regExMatches: $regExMatches")
        } else {
          if (regExMatches.exists(_._1 == value.get)) {
            val max = regExMatches.map(_._3).reduce((a1, a2) => a1.max(a2))
            if (regExMatches.filter(_._3 == max).exists(_._1 == value.get)) {
              println(s"ok: $strKey -> ${value.get}")
              true
            } else {
              failure(s"value has not maximum score - strKey: $strKey; value: $value; regExMatches: $regExMatches; max: $max")
            }
          } else {
            failure(s"value is not contained in regex matches - strKey: $strKey; value: $value; regExMatches: $regExMatches")
          }
        }
      }
    })

  }
}
