package eu.swdev.i18n

import scala.collection.mutable.{Map => MMap}
import java.util.Locale
import scala.collection.GenTraversable
import java.net.URL

/**
 *
 */
object Analyzer {

  def analyze(classLoader: ClassLoader, resourcePath: String, locales: Locale*): AnalyzeResult = {
    val resources = ResourcesLoader.loadResources(classLoader, resourcePath, locales: _*)
    analyzeKeys(resources)
  }

  case class AnalyzeResult(simpleKeys: Set[String], lookupKeys: Set[String], args: Map[String, Int], missingKeys: Map[Locale, Set[String]], ambiguouslyUsedKey: Set[String])

  def analyzeKeys(aggregated: Map[Locale, ResourceEntries]): AnalyzeResult = {
    val (allKeyIds1, missingKeyIds1) = analyzeKeys(aggregated, true)
    val (allKeyIds2, missingKeyIds2) = analyzeKeys(aggregated, false)

    val missingKeyIds: Map[Locale, Set[String]] = aggregated.keys.map(l => l -> (missingKeyIds1(l) ++ missingKeyIds2(l))).toMap
    val ambiguouslyUsedKeys: Set[String] = allKeyIds1.intersect(allKeyIds2)

    // maps key ids to the maximum number of arguments
    val args: Map[String,Int] = aggregated.values.foldLeft(Map.empty[String, Int])((b, r) => r.entries.foldLeft(b)((b1, e) => {
      val id = e.key.id
      val l = e.msg.getFormatsByArgumentIndex.length
      if (b1.contains(id) && b1(id) >= l) {
        b1
      } else {
        b1 + (id -> l)
      }
    }))

    AnalyzeResult(allKeyIds1, allKeyIds2, args, missingKeyIds, ambiguouslyUsedKeys)
  }

  def analyzeKeys(aggregated: Map[Locale, ResourceEntries], simpleNotLookup: Boolean): (Set[String], Map[Locale, Set[String]]) = {
    // maps locales to the key ids that are defined for them
    val keyIds: Map[Locale, Set[String]] = aggregated.mapValues(_.entries.map(_.key).filter(_ match {
      case k: SimpleEntryKey => simpleNotLookup
      case k: LookupEntryKey => !simpleNotLookup}
    ).map(_.id).toSet)
    val allKeyIds: Set[String] = keyIds.values.foldLeft(Set.empty[String])((s, kids) => s ++ kids)

    // maps locales to the key ids that are NOT defined for them
    val missingKeyIds: Map[Locale, Set[String]] = keyIds.mapValues(allKeyIds -- _)

    (allKeyIds, missingKeyIds)
  }

}
