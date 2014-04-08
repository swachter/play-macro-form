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

  /**
   * Represents how many arguments a message has and if it contains markup.
   *
   * @param args
   * @param isMarkup
   */
  case class MsgSignature(args: Int, isMarkup: Boolean)

  case class AnalyzeResult(simpleKeys: Set[String], lookupKeys: Set[String], signatures: Map[String, MsgSignature], missingKeys: Map[Locale, Set[String]], ambiguouslyUsedKeys: Set[String])

  def analyzeKeys(aggregated: Map[Locale, ResourceEntries]): AnalyzeResult = {
    val (allKeyIds1, missingKeyIds1) = analyzeKeys(aggregated, true)
    val (allKeyIds2, missingKeyIds2) = analyzeKeys(aggregated, false)

    val missingKeyIds: Map[Locale, Set[String]] = aggregated.keys.map(l => l -> (missingKeyIds1(l) ++ missingKeyIds2(l))).toMap
    val ambiguouslyUsedKeys: Set[String] = allKeyIds1.intersect(allKeyIds2)

    // maps key id to message signatures
    // the map is calculated by a nested fold over all resources and their entries
    // the final signatures contain the maximum number of arguments a message has in any local and if a message contains
    // markup in any locale.
    val signatures: Map[String, MsgSignature] = aggregated.values.foldLeft(Map.empty[String, MsgSignature])((b, r) => r.entries.foldLeft(b)((b1, e) => {
      val id = e.key.id
      val l = e.msg.getFormatsByArgumentIndex.length
      val old = b1.get(id)
      // check if a signature has already been determined and if that signature has more arguments and is alread markup
      // -> in that case the signature stays unchanged
      if (old.map(s => s.args >= l && s.isMarkup).getOrElse(false)) {
        b1
      } else {
        b1 + (id -> old.map(s => MsgSignature(s.args.max(l), s.isMarkup || e.isMarkup)).getOrElse(MsgSignature(l, e.isMarkup)))
      }
    }))

    AnalyzeResult(allKeyIds1, allKeyIds2, signatures, missingKeyIds, ambiguouslyUsedKeys)
  }

  /**
   *
   * @param aggregated
   * @param simpleNotLookup determines if simple entries or if lookup entries are considered
   * @return
   */
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
