package eu.swdev.i18n

import scala.collection.mutable.{Map => MMap}
import java.util.Locale

/**
  */
object ResourcesLoader {

  type LoadResult = Either[List[String], List[EntryLine]]
  type LocaleLoader = Option[Locale] => LoadResult

  def loadEntries(classLoader: ClassLoader, resourcePath: String, locales: List[Locale]): Map[Locale, Map[EntryName, Entry]] = {
    val loader = new ClassLoaderEntryLinesLoader(classLoader, resourcePath)
    loadEntryLines(loader, locales: _*) match {
      case Left(l) => throw new Exception(l.mkString("\n"))
      case Right(entryLinesByLocale) => {
        val analyzeResult = Analyzer.analyzeEntryLinesOfAllLocales(entryLinesByLocale)
        // map the iterator of locales into an iterator of tuples (locale -> Map[EntryName, Entry]) then convert it into a map
        entryLinesByLocale.keys.map(locale => locale -> {
          val entryNames = analyzeResult.resultsOfOneLocale(locale).ordered
          val grouped = analyzeResult.resultsOfOneLocale(locale).grouped
          // fold over the ordered entry names for the current locale and aggregate then entry map
          entryNames.foldLeft(Map.empty[EntryName, Entry])((b, entryName) => {

            def entryLineValue(line: EntryLine, tpe: EntryType): Entry = line.value match {
              case MsgEntryLineValue(format, isMarkup) => MsgEntry(tpe, format, isMarkup)
              case LinkEntryLineValue(name) => b(name)
            }

            val entryLines = grouped(entryName)

            val entry = analyzeResult.types(entryName) match {
              case entryType: MsgEntryType => {
                // there must be exactly one entry line because the entry has the MsgEntryType
                entryLineValue(entryLines.head, entryType)
              }
              case entryType: LookupEntryType => {
                // fold over all lines starting with an empty entry, and incorporate the key-value pairs
                entryLines.foldLeft(entryType.emptyEntry)((b1, line) => line.id match {
                  case LookupEntryLineId(_, key) => b1 + (key, entryLineValue(line, entryType))
                })
              }
            }

            b + (entryName -> entry)

          })
        }).toMap
      }
    }
  }

  def loadEntryLines(loader: LocaleLoader, locales: Locale*): Either[List[String], Map[Locale, EntryLines]] = {
    val cache = new LocaleLoaderCache(loader)
    val tmp = (for {
      locale <- locales
    } yield {
      // aggregate the resources that belong to the locale starting with the resource that belongs to all locales (i.e. None).
      val ll: List[Option[Locale]] = None :: localeList(locale).map(Some(_))
      val loadResults: List[LoadResult] = ll.map(cache.get(_))

      locale -> (if (loadResults.exists(_.isLeft)) {
        Left(loadResults.collect{ case Left(l) => l }.flatten)
      } else {
        Right(loadResults.collect{ case Right(r) => r }.flatten)
      })
    }).toMap

    if (tmp.values.exists(_.isLeft)) {
      Left(tmp.values.collect{ case Left(l) => l}.toList.flatten)
    } else {
      Right(tmp.mapValues(either => EntryLines.empty ++ either.right.get))
    }
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

  class LocaleLoaderCache(loader: LocaleLoader) {

    val loaded: MMap[Option[Locale], LoadResult] = MMap.empty

    def get(locale: Option[Locale]): LoadResult = {
      if (!loaded.contains(locale)) {
        loaded(locale) = loader(locale)
      }
      loaded(locale)
    }

  }

  class ClassLoaderEntryLinesLoader(classLoader: ClassLoader, resourcePath: String) extends LocaleLoader {
    override def apply(v1: Option[Locale]): LoadResult = {
      val path = v1.map(l => s"$resourcePath.${l.toString}").getOrElse(resourcePath)
      import scala.collection.JavaConverters._
      val tmp = (for {
        url <- classLoader.getResources(path).asScala
      } yield {
        EntryLinesParser.parsePhraseLines(url)
      }).toList
      if (tmp.exists(_.isLeft)) {
        Left(tmp.collect{ case Left(l) => s"msg: ${l._1}; line: ${l._2.pos.line}; column: ${l._2.pos.column}; url: ${l._3}" })
      } else {
        Right(tmp.map(_.right.get).flatten)
      }
    }
  }
}
