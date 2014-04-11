package eu.swdev.i18n

import scala.collection.mutable.{Map => MMap}
import java.util.Locale

/**
 *
 */
object Analyzer {

  type MissingNames = Map[Locale, Set[EntryName]]
  type ConflictingTypes = Map[EntryName, List[(Locale, List[EntryType])]]

  type UnprocessedNames = Map[EntryName, Set[EntryName]]
  type TypesByName = Map[EntryName, List[EntryType]]

  /**
   * The result of analyzing a map of locales and their entry lines.
   * 
   * @param types Maps entry names to their entry type.
   * @param missing Informs about missing entry definitions, i.e. entry definitions that are not present in some locales.
   *                For each locale the set of missing entry names is given.
   * @param conflicting Informs about conflicting entry definitions, i.e. entry definitions that have different entry types for different locales.
   *                    For each entry name a list of locales and the entry types that exist for that entry name and locale is given.
   * @param resultsOfOneLocale Holds the separate analysis results for each locale.
   */
  case class AnalysisResultOfAllLocales(types: Map[EntryName, EntryType], missing: MissingNames, conflicting: ConflictingTypes, resultsOfOneLocale: Map[Locale, AnalysisResultOfOneLocale])

  /**
   * The result of analyzing a single list of entry lines.
   *
   * @param types Maps entry names to their entry type
   * @param ordered Lists entry names according to their dependencies. Entries that are named first do not depend on entries named later.
   * @param unprocessed Informs about unprocessed entities, i.e. entities that depend on other entities and whose processing
   *                    result was not available.
   * @param grouped Maps entry names to the list of entry lines that belong to that entry name
   */
  case class AnalysisResultOfOneLocale(types: TypesByName, ordered: List[EntryName], unprocessed: UnprocessedNames, grouped: Map[EntryName, List[EntryLine]])

  def analyze(classLoader: ClassLoader, resourcePath: String, locales: Locale*): AnalysisResultOfAllLocales = {
    val loader = new ResourcesLoader.ClassLoaderEntryLinesLoader(classLoader, resourcePath)
    val resources = ResourcesLoader.loadEntryLines(loader, locales: _*)
    analyzeEntryLinesOfAllLocales(resources.right.get)
  }

  /**
   * Analyzes a map of EntryLines.
   *
   *
   * @param input
   * @return
   */
  def analyzeEntryLinesOfAllLocales(input: Map[Locale, EntryLines]): AnalysisResultOfAllLocales = {

    // analyze each EntryLines instance separately
    val resultsOfOneLocale: Map[Locale, AnalysisResultOfOneLocale] = input.mapValues(el => analyzeEntryLinesOfOneLocale(el.entries))
    
    val locales: List[Locale] = input.keys.toList
    val entryTypes: Map[Locale, TypesByName] = resultsOfOneLocale.mapValues(_.types)

    // collect the entry names of all entry types that resulted from the separate analysis of each EntryLines instance
    val allNames: Set[EntryName] = entryTypes.values.flatMap(_.keys).toSet

    // for each locale calculate the set of entry names for which there is no entry type known in that locale
    // -> missing = allNames - <the names of all known entry types of that locale>
    val missingNames: MissingNames = entryTypes.mapValues(m => allNames -- m.keys)

    // the entry names for which there are entry types in all locales
    // -> commonNames = allName -- <the missing names in all locales>
    val commonNames: Set[EntryName] = allNames -- missingNames.values.flatMap(_.iterator)

    // aggregate the entry types that were determined for each locale separately and collect all conflicts, i.e. situations
    // where there is more than one entry type for an entry.
    // -> fold over the common names starting with empty maps for entry types and conflicts
    val (allTypes, conflicts) = commonNames.foldLeft((Map.empty[EntryName, EntryType], Map.empty[EntryName, List[(Locale, List[EntryType])]]))((b, entryName) => {

      // for each entry name: collect all known entry types for all locales for that entry name
      val etl: List[EntryType] = locales.flatMap(entryTypes(_)(entryName))
      val unified = unifyEntryTypes(etl)
      // augment the conflicts map if more than one entry type is found
      val newConflicts = if (unified.tail.isEmpty) {
        b._2
      } else {
        b._2 + (entryName -> (locales.map(l => (l, entryTypes(l)(entryName)))))
      }
      // augment the entry types mapping by the first found entry type
      (b._1 + (entryName -> unified.head), newConflicts)
    })

    AnalysisResultOfAllLocales(allTypes, missingNames, conflicts, resultsOfOneLocale)
  }

  /**
   * Analyzes a list of entry lines.
   *
   * @param entries
   * @return
   */
  def analyzeEntryLinesOfOneLocale(entries: List[EntryLine]): AnalysisResultOfOneLocale = {

    val (orderedNames, unprocessed, grouped) = orderEntries(entries)

    // calculate the map of entry types by folding over the ordered list of entry names and starting with an empty map
    val entryTypes: TypesByName = orderedNames.foldLeft(Map.empty[EntryName, List[EntryType]])((accu, entryName) => {
      // for each entryName map its list of entry lines into a list of entry types
      val etl: List[EntryType] = grouped(entryName).map(line => line.id match {
        case SimpleEntryLineId(_) => {
          line.value match {
            case MsgEntryLineValue(format, isMarkup) => MsgEntryType(format, isMarkup)
            case LinkEntryLineValue(name) => accu(name).head // arbitrary choice: use the first known entry type
          }
        }
        case TreeEntryLineId(_, _) => {
          line.value match {
            case MsgEntryLineValue(format, isMarkup) => TreeEntryType(MsgEntryType(format, isMarkup))
            case LinkEntryLineValue(name) => TreeEntryType(accu(name).head) // arbitrary choice: use the first known entry type
          }
        }
        case MapEntryLineId(_, _) => {
          line.value match {
            case MsgEntryLineValue(format, isMarkup) => MapEntryType(MsgEntryType(format, isMarkup))
            case LinkEntryLineValue(name) => MapEntryType(accu(name).head) // arbitrary choice: use the first known entry type
          }
        }
      })
      // store the unified entry types list in the accu
      accu + (entryName -> unifyEntryTypes(etl))
    })
    AnalysisResultOfOneLocale(entryTypes, orderedNames, unprocessed, grouped)
  }

  /**
   * Unifies a list of entry types.
   *
   * Two entry types can be unified to a single entry type if they are both of the same "kind", i.e. if both of the same type
   * and in case of a MapEntryType or a TreeEntryType their nested types can be unified. MessageEntryTypes are unified by taking the
   * maximum number of arguments and by doing an or-conjunction of their isMarkup properties.
   *
   * @param list a non-empty list
   * @return
   */
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
   * Calculate a sorted list of the entry names that appear in a list of entry lines.
   * 
   * The list is sorted such a way that entries named first do not depend on entries named later.
   * The entries are ordered by calculating the maximum number of link values one must follow starting at an entry value
   * in order to reach a simple entry value.
   *
   * @param lines
   * @return a tuple containing a list of ordered entry names and a map of entry names that could not be resolved
   */
  def orderEntries(lines: List[EntryLine]): (List[EntryName], UnprocessedNames, Map[EntryName, List[EntryLine]]) = {

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

    /**
     * Recursively process the dependency infos until it does not change.
     *
     * @param di
     * @return
     */
    def process(di: DependencyInfos): DependencyInfos = {
      // calculate the new dependency infos by mapping the values of the current dependency infos:
      //   - a depth is kept
      //   - a list of links
      //          is mapped into a depth if the depth of all the link targets is known
      //          is kept otherwise
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

    val grouped: Map[EntryName, List[EntryLine]] = lines.groupBy(_.id.name)

    // entry name -> the list of link values resulting from the mapped and filtered entry lines
    val groupedLinks: Map[EntryName, List[LinkEntryLineValue]] = grouped.mapValues(_.map(_.value).collect{ case l: LinkEntryLineValue => l })

    // the initial dependency info for all groups:
    // - Depth(0) for all groups that have no links
    // - Links(...) for all groups that have links
    val initial: DependencyInfos = groupedLinks.mapValues(_ match {
      case Nil => Depth(0)
      case l => Links(l.map(_.name))
    })

    // recursively process the dependency infos
    val di = process(initial)

    // collect a list of (EntryName/Depth) pairs, sort them by their depth, and return the entry names
    val orderedEntryNames: List[EntryName] = di.toList.collect{ case (n: EntryName, d: Depth) => (n, d) }.sortWith((l, r) => l._2.depth < r._2.depth).map(_._1)

    // collect the (EntryName/Links) pairs and map the links to the set of those entry names for which no depth is known
    val unresolved: UnprocessedNames = di.collect{ case (n: EntryName, l: Links) => (n, l.list.filter(nn => !di.get(nn).isDefined || !di(nn).isDepth).toSet)}
    (orderedEntryNames, unresolved, grouped)
  }
}
