package eu.swdev.i18n

import scala.collection.mutable.{Map => MMap}
import java.util.Locale

/**
  */
object ResourcesLoader {

  def buildMaps(classLoader: ClassLoader, resourcePath: String, locales: Locale*): (Map[Locale, Map[String, MsgFormat]], Map[Locale, Map[String, MsgLookup.KeyValueTree]]) = {
    val resources = loadResources(classLoader, resourcePath, locales: _*)
    val zero = (Map.empty[String, MsgFormat], Map.empty[String, MsgLookup.KeyValueTree])
    val both = resources.mapValues(_.entries.foldLeft(zero)((a, e) => {
      e.key match {
        case SimpleEntryKey(id) => {
          (a._1 + (id -> MsgFormat(e.msg, e.isMarkup)), a._2)
        }
        case LookupEntryKey(id, path) => {
          (a._1, a._2 + (id -> (a._2.getOrElse(id, MsgLookup.KeyValueTree.empty)(path) = MsgFormat(e.msg, e.isMarkup))))
        }
      }

    }))
    (both.mapValues(_._1), both.mapValues(_._2))
  }

  def loadResources(classLoader: ClassLoader, resourcePath: String, locales: Locale*): Map[Locale, ResourceEntries] = {
    val resourceLoader = new ResourceLoader(classLoader, resourcePath)
    (for {
      locale <- locales
    } yield {
      // aggregate the resources that belong to the locale starting with the resource that belongs to all locales.
      locale -> localeList(locale).foldLeft(resourceLoader.load(None))((r, l) => r.add(resourceLoader.load(Some(l))))
    }).toMap
  }

  /**
   * Returns a list of 0, 1, or 2 locales.
   *
   * The returned list contains less specific locales before more specific locales.
   *
   * @param locale
   * @return
   */
  private def localeList(locale: Locale): List[Locale] = {
    if (locale.getLanguage.isEmpty && locale.getCountry.isEmpty) {
      Nil
    } else if (locale.getCountry.isEmpty) {
      locale :: Nil
    } else {
      new Locale(locale.getLanguage) :: locale :: Nil
    }
  }

  class ResourceLoader(classLoader: ClassLoader, resourcePath: String) {

    val loadedResources: MMap[Option[Locale], ResourceEntries] = MMap.empty

    def load(locale: Option[Locale]): ResourceEntries = {
      if (!loadedResources.contains(locale)) {
        loadedResources(locale) = doLoad(locale)
      }
      loadedResources(locale)
    }

    private def doLoad(locale: Option[Locale]): ResourceEntries = {
      val path = locale.map(l => s"$resourcePath.${l.toString}").getOrElse(resourcePath)
      import scala.collection.JavaConverters._
      val urls = classLoader.getResources(path).asScala
      ResourceEntries(urls)
    }

  }

}
