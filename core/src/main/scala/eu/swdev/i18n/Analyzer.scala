package eu.swdev.i18n

import scala.collection.mutable.{Map => MMap}
import java.util.Locale
import scala.collection.GenTraversable
import java.net.URL

/**
 *
 */
object Analyzer {

  type MissingEntries = Map[Locale, Set[EntryName]]
  type ConflictingEntries = Map[EntryName, List[(Locale, List[EntryType])]]

  type UnresolvedEntries = Map[EntryName, Set[EntryName]]
  type EntryTypes = Map[EntryName, List[EntryType]]

  def analyze(classLoader: ClassLoader, resourcePath: String, locales: Locale*): AnalyzeResult = {
    val resources = ResourcesLoader.loadResources(classLoader, resourcePath, locales: _*)
    analyzeResourceEntries(resources)
  }

  case class AnalyzeResult(types: Map[EntryName, EntryType], unresolved: Map[Locale, UnresolvedEntries], missing: MissingEntries, conflicting: ConflictingEntries)

  /**
   * Analyzes a map of ResourceEntries.
   *
   *
   * @param input
   * @return
   */
  def analyzeResourceEntries(input: Map[Locale, EntryLines]): AnalyzeResult = {

    val tmp: Map[Locale, (EntryTypes, UnresolvedEntries)] = input.mapValues(entries => {
      val (entryNames, unresolved) = orderEntries(entries.entries)
      val entryTypes = determineEntryTypesForOneLocale(entries.entries, entryNames)
      (entryTypes, unresolved)
    })

    val (types, missing, conflicting) = determineEntryTypesForAllLocales(tmp.mapValues(_._1))

    AnalyzeResult(types, tmp.mapValues(_._2), missing, conflicting)
  }

  /**
   *
   * @param input
   * @return a tuple consisting of
   *         - a map of entry types
   *         - information about missing entries: a map of locales to the sets of missing entry names
   *         - information about conflicting entry types: lists of the different entry types of an entry
   */
  def determineEntryTypesForAllLocales(input: Map[Locale, EntryTypes]): (Map[EntryName, EntryType], MissingEntries, ConflictingEntries) = {
    val locales: List[Locale] = input.keys.toList
    val allNames: Set[EntryName] = input.values.flatMap(_.keys).toSet
    val missingNames: Map[Locale, Set[EntryName]] = input.mapValues(m => allNames -- m.keys)
    val commonNames: Set[EntryName] = allNames -- missingNames.values.flatMap(_.iterator)
    val both = commonNames.foldLeft((Map.empty[EntryName, EntryType], Map.empty[EntryName, List[(Locale, List[EntryType])]]))((b, entryName) => {
      val entryTypes: List[EntryType] = locales.flatMap(input(_)(entryName))
      val unified = unifyEntryTypes(entryTypes)
      val newConflicts = if (unified.tail.isEmpty) {
        b._2
      } else {
        b._2 + (entryName -> (locales.map(l => (l, input(l)(entryName)))))
      }
      (b._1 + (entryName -> unified.head), newConflicts)
    })
    (both._1, missingNames, both._2)
  }

  /**
   * 
   * @param entries
   * @param names an ordered list of entry names; entries named first do not depend on entries named later
   * @return
   */
  def determineEntryTypesForOneLocale(entries: List[EntryLine], names: List[EntryName]): EntryTypes = {

    val grouped: Map[EntryName, List[EntryLine]] = entries.groupBy(_.id.name)

    names.foldLeft(Map.empty[EntryName, List[EntryType]])((accu, entryName) => {
      val entryTypes: List[EntryType] = grouped(entryName).map(resourceEntry => resourceEntry.id match {
        case SimpleEntryLineId(_) => {
          resourceEntry.value match {
            case MsgEntryLineValue(format, isMarkup) => MsgEntryType(format, isMarkup)
            case LinkEntryLineValue(name) => accu(name).head
          }
        }
        case TreeEntryLineId(_, _) => {
          resourceEntry.value match {
            case MsgEntryLineValue(format, isMarkup) => TreeEntryType(MsgEntryType(format, isMarkup))
            case LinkEntryLineValue(name) => TreeEntryType(accu(name).head)
          }
        }
        case MapEntryLineId(_, _) => {
          resourceEntry.value match {
            case MsgEntryLineValue(format, isMarkup) => MapEntryType(MsgEntryType(format, isMarkup))
            case LinkEntryLineValue(name) => MapEntryType(accu(name).head)
          }
        }
      })

      val unifiedEntryTypes: List[EntryType] = unifyEntryTypes(entryTypes)
      accu + (entryName -> unifiedEntryTypes)
    })
  }

  def unifyEntryTypes(list: List[EntryType]): List[EntryType] = {
    def unify(et1: EntryType, et2: EntryType): Option[EntryType] = (et1, et2) match {
      case (MsgEntryType(args1, isMarkup1), MsgEntryType(args2, isMarkup2)) => Some(MsgEntryType(args1.max(args2), isMarkup1 || isMarkup2))
      case (TreeEntryType(mt1), TreeEntryType(mt2)) => unify(mt1, mt2).map(TreeEntryType(_))
      case (MapEntryType(mt1), MapEntryType(mt2)) => unify(mt1, mt2).map(MapEntryType(_))
      case _ => None
    }
    list.tail.foldLeft(List(list.head))((accu, entryType) => {
      if (accu.exists(unify(_, entryType).isDefined)) {
        accu.map(et => unify(et, entryType).getOrElse(et))
      } else {
        entryType :: accu
      }
    })
  }

  //
  //
  //

  /**
   * Calculate a sorted list of entry names such that entries named first do not depend on entries named later.
   *
   * The entries are ordered by calculating the maximum number of link values one must follow starting at an entry value
   * in order to reach a simple entry value.
   *
   * @param entries
   * @return a tuple containing a list of ordered entry names and a map of entry names that could not be resolved
   */
  def orderEntries(entries: List[EntryLine]): (List[EntryName], UnresolvedEntries) = {

    type DependencyInfos = Map[EntryName, DependencyInfo]

    sealed trait DependencyInfo {
      def isDepth: Boolean
      def depth: Int
    }

    case class Depth(depth: Int) extends DependencyInfo {
      def isDepth = true
    }

    case class Links(list: List[EntryName]) extends DependencyInfo {
      def isDepth = false
      def depth = throw new UnsupportedOperationException
    }

    def process(di: DependencyInfos): DependencyInfos = {
      val newDi = di.mapValues(_ match {
        case d: Depth => d
        case l@Links(list) => {
          if (list.forall(n => di.get(n).isDefined && di(n).isDepth)) {
            Depth(1 + list.map(di(_).depth).max)
          } else {
            l
          }
        }
      })
      if (di == newDi) {
        di
      } else {
        process(newDi)
      }
    }

    val grouped: Map[EntryName, List[LinkEntryLineValue]] = entries.groupBy(_.id.name).mapValues(_.map(_.value).collect{ case l: LinkEntryLineValue => l })
    val initial: DependencyInfos = grouped.mapValues(_ match {
      case Nil => Depth(0)
      case l => Links(l.map(_.name))
    })

    val di = process(initial)
    val orderedEntryNames: List[EntryName] = di.toList.collect{ case (n: EntryName, d: Depth) => (n, d) }.sortWith((l, r) => l._2.depth < r._2.depth).map(_._1)
    val unresolved: UnresolvedEntries = di.collect{ case (n: EntryName, l: Links) => (n, l.list.filter(nn => !di.get(nn).isDefined || !di(nn).isDepth).toSet)}
    (orderedEntryNames, unresolved)
  }
}
