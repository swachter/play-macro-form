package eu.swdev.config

import java.util.regex.Pattern

/**
 * Provides a key-value store that uses hierarchically structured keys and supports wildcard entries. This kind of store
 * is called a KeyValueTree.
 *
 * A key is represented by a possibly empty sequence of steps. Two types of keys must be distinguished:
 *
 * 1. definition keys, i.e. keys that are used when a KeyValueTree is defined
 * 2. retrieval key, i.e. keys that are used when a value is retrieved from the KeyValueTree
 *
 * Retrieval keys can contain normal steps only. A normal step is a step that has a specific step value. Definitions keys
 * can contain normal steps and wildcard steps. There are two kinds of wildcard steps: single wildcard steps that match
 * a single arbitrary step value and multi wildcard steps that match an arbitrary number (0 included!) of arbitrary step
 * values. A definition key can contain many wildcard steps.
 *
 * In the following example single wildcard steps are identified by a '?' character and multi-wildcard steps are identified
 * by a '*' character:
 *
 * <code>
 * a=1
 * a.*=2
 * ?.a=3
 * ?.a.?=4
 * </code>
 *
 * The first line defines an ordinary key-value mapping. The second line defines a key-value mapping for any keys that
 * start with an 'a' followed by zero or more steps. Note that the retrieval key 'a' would match the definition key
 * of line 1 and line 2. The third line defines a value for any keys that start with an
 * arbitrary step followed by an 'a'-step. Finally the last line defines a value for all keys starting with an arbitrary
 * key followed by an 'a'-step and another arbitrary step.
 *
 * If a retrieval key matches several definition keys then the matches are scored and the match with the maximum score
 * is selected. The score of a match is equal to the sum of the scores of the matching steps. Each normal step is counted
 * as 10, each single wildcardstep is counted as 1, and each multi-wildcard step is counted as 0. The scoring function
 * prefers more specific definition keys. Additionally, if several definition keys have the same score then the definition
 * key that ends with a normal step is preferred. If there is still an ambiguity then it is undefined which definition
 * key is matched, i.e. which value is selected.
 */
trait KeyValueTreeModule {

  type Key
  type Step
  type Value

  def splitKey(key: Key): List[Step]
  def isAsteriskStep(step: Step): Boolean
  def isQuestionMarkStep(step: Step): Boolean

  import KeyValueTree._

  /**
   *
   */
  case class KeyValueTree(asterisk: Option[KeyValueTree], questionMark: Option[KeyValueTree], map: Map[Step, KeyValueTree], value: Option[Value]) {

    def getValue(key: Key): Option[Value] = {

      def doGetValue(steps: List[Step], matchStates: Seq[MatchState]): Option[Value] = {
        steps match {
          case step :: tail => doGetValue(tail, matchStates.flatMap(_.step(step)))
          case _ => matchStates.flatMap(_.expand).foldLeft[(Int, Option[Value])]((-1, None))((accu, matchState) => {
            if (matchState.value.isDefined && matchState.weight > accu._1) {
              (matchState.weight, matchState.value)
            } else {
              accu
            }
          })._2
        }
      }

      doGetValue(splitKey(key), Seq(TreeMatchState(0, this)))
    }

    /**
     * Returns a new key-value tree with the given value for the given key.
     *
     * @param key
     * @param value
     * @return
     */
    def update(key: Key, value: Value): KeyValueTree = {
      def doUpdate(oldTree: KeyValueTree, steps: List[Step]): KeyValueTree = steps match {
        case step :: tail => if (isAsteriskStep(step)) {
          oldTree.copy(asterisk = Some(doUpdate(oldTree.asterisk.getOrElse(empty), tail)))
        } else if (isQuestionMarkStep(step)) {
          oldTree.copy(questionMark = Some(doUpdate(oldTree.questionMark.getOrElse(empty), tail)))
        } else {
          oldTree.copy(map = oldTree.map + (step -> doUpdate(oldTree.map.getOrElse(step, empty), tail)))
        }
        case _ => oldTree.copy(value = Some(value))
      }
      doUpdate(this, splitKey(key))
    }

  }

  object KeyValueTree {

    val empty = KeyValueTree(None, None, Map.empty, None)

    def apply(keyValues: Seq[(Key, Value)]): KeyValueTree = {
      keyValues.foldLeft(empty)((tree, keyValue) => tree(keyValue._1) = keyValue._2)
    }

    trait MatchState {
      def weight: Int
      def value: Option[Value]
      def step(step: Step): Seq[MatchState]
      def expand: Seq[MatchState]
    }

    case class TreeMatchState(weight: Int, tree: KeyValueTree) extends MatchState {
      def step(step: Step): Seq[MatchState] = {
        val b = Seq.newBuilder[MatchState]
        tree.asterisk.foreach(AsteriskMatchState(weight, _).step(step).foreach(b += _))
        tree.questionMark.foreach(b += TreeMatchState(weight + 1, _))
        tree.map.get(step).foreach(b += TreeMatchState(weight + 10, _))
        b.result()
      }
      def value = tree.value
      def expand: Seq[MatchState] = {
        val b = Seq.newBuilder[MatchState]
        b += this
        tree.asterisk.foreach(AsteriskMatchState(weight, _).expand.foreach(b += _))
        b.result
      }
    }

    case class AsteriskMatchState(weight: Int, tree: KeyValueTree) extends MatchState {
      def value = None
      def step(step: Step): Seq[MatchState] = {
        val b = Seq.newBuilder[MatchState]
        b += this
        TreeMatchState(weight, tree).step(step).foreach(b += _)
        b.result()
      }
      def expand: Seq[MatchState] = {
        val b = Seq.newBuilder[MatchState]
        b += this
        TreeMatchState(weight, tree).expand.foreach(b += _)
        b.result
      }
    }

  }
}

trait StringKeyValueTreeModule extends KeyValueTreeModule {

  type Key = String
  type Step = String

  val dotPattern = Pattern.compile("\\.")

  override def isQuestionMarkStep(step: StringKeyValueTreeModule#Step): Boolean = step == "?"

  override def isAsteriskStep(step: StringKeyValueTreeModule#Step): Boolean = step == "*"

  override def splitKey(key: StringKeyValueTreeModule#Key): List[StringKeyValueTreeModule#Step] = {
    // if the key starts with a dot or ends with a dot then an empty string is returned as the first or as the last
    // element of the returned split array, respectively.
    dotPattern.split(key, -1).toList
  }
}
