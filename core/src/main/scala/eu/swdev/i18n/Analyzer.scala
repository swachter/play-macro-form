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

  case class MsgSignature(args: Int, isMarkup: Boolean)

  case class AnalyzeResult(simpleKeys: Set[String], lookupKeys: Set[String], signatures: Map[String, MsgSignature], missingKeys: Map[Locale, Set[String]], ambiguouslyUsedKey: Set[String])

  def analyzeKeys(aggregated: Map[Locale, ResourceEntries]): AnalyzeResult = {
    val (allKeyIds1, missingKeyIds1) = analyzeKeys(aggregated, true)
    val (allKeyIds2, missingKeyIds2) = analyzeKeys(aggregated, false)

    val missingKeyIds: Map[Locale, Set[String]] = aggregated.keys.map(l => l -> (missingKeyIds1(l) ++ missingKeyIds2(l))).toMap
    val ambiguouslyUsedKeys: Set[String] = allKeyIds1.intersect(allKeyIds2)

    // maps key ids to the maximum number of arguments
    val signatures: Map[String, MsgSignature] = aggregated.values.foldLeft(Map.empty[String, MsgSignature])((b, r) => r.entries.foldLeft(b)((b1, e) => {
      val l = e.msg.getFormatsByArgumentIndex.length
      e.key match {
        case SimpleEntryKey(id) => {
          val old = b1.get(id)
          if (old.map(_.args >= l).getOrElse(false)) {
            b1
          } else {
            b1 + (id -> old.map(_.copy(args = l)).getOrElse(MsgSignature(l, false)))
          }
        }
        case LookupEntryKey(id, path) => {
          val old = b1.get(id)
          if (old.map(s => s.args >= l && s.isMarkup).getOrElse(false)) {
            b1
          } else {
            b1 + (id -> old.map(s => MsgSignature(s.args.max(l), true)).getOrElse(MsgSignature(l, true)))
          }

        }
      }


    }))

    AnalyzeResult(allKeyIds1, allKeyIds2, signatures, missingKeyIds, ambiguouslyUsedKeys)
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
