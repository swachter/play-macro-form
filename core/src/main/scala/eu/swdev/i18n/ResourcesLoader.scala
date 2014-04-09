package eu.swdev.i18n

import scala.collection.mutable.{Map => MMap}
import java.util.Locale

/**
  */
object ResourcesLoader {

  def load(classLoader: ClassLoader, resourcePath: String, locales: Locale*): Map[Locale, Map[EntryName, ResValue]] = {
    val resources: Map[Locale, ResourceEntries] = loadResources(classLoader, resourcePath, locales: _*)
    resources.mapValues(entries => {
      val (entryNames, unresolved) = Analyzer.orderEntries(entries.entries)
      val entryTypes = Analyzer.determineEntryTypesForOneLocale(entries.entries, entryNames)
      val grouped = entries.entries.groupBy(_.key.name)
      entryNames.foldLeft(Map.empty[EntryName, ResValue])((b, entryName) => {
        grouped(entryName).foldLeft(b)((b1, re) => {
          val resValue: ResValue = re.value match {
            case MsgEntryValue(format, isMarkup) => MsgResValue(format, isMarkup)
            case LinkEntryValue(name) => b1(name)
          }
          re.key match {
            case SimpleEntryKey(_) => b1 + (entryName -> resValue)
            case TreeEntryKey(_, path) => {
              val treeValue = b1.getOrElse(entryName, TreeResValue(ResTrees.KeyValueTree.empty)).asInstanceOf[TreeResValue]
              b1 + (entryName -> TreeResValue(treeValue.tree(path) = resValue))
            }
          }
        })
      })
    })
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
