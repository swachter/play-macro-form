package eu.swdev.config

import java.util.regex.Pattern

trait KeyValueTreeModule {

  type Key
  type Step
  type Value

  def splitKey(key: Key): List[Step]
  def isAsteriskStep(step: Step): Boolean
  def isQuestionMarkStep(step: Step): Boolean

  import KeyValueTree._

  /**
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

  }

  object KeyValueTree {

    val empty = KeyValueTree(None, None, Map.empty, None)

    def apply(keyValues: Seq[(Key, Value)]): KeyValueTree = {
      keyValues.foldLeft(empty)((tree, keyValue) => {
        val steps = splitKey(keyValue._1)
        augmentTree(tree, steps, keyValue._2)
      })
    }

    def augmentTree(oldTree: KeyValueTree, steps: Seq[Step], value: Value): KeyValueTree = {
      if (steps.isEmpty) {
        oldTree.copy(value = Some(value))
      } else {
        val step = steps.head
        if (isAsteriskStep(step)) {
          oldTree.copy(asterisk = Some(augmentTree(oldTree.asterisk.getOrElse(empty), steps.tail, value)))
        } else if (isQuestionMarkStep(step)) {
          oldTree.copy(questionMark = Some(augmentTree(oldTree.questionMark.getOrElse(empty), steps.tail, value)))
        } else {
          oldTree.copy(map = (oldTree.map + (step -> augmentTree(oldTree.map.getOrElse(step, empty), steps.tail, value))))
        }
      }
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

  override def splitKey(key: StringKeyValueTreeModule#Key): List[StringKeyValueTreeModule#Step] = dotPattern.split(key, -1).toList
}
