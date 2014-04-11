package eu.swdev.i18n

import scala.util.parsing.combinator.RegexParsers
import java.text.MessageFormat
import java.net.URL
import java.io.InputStream
import scala.util.parsing.input.{CharSequenceReader, StreamReader}
import scala.io.Source
import scala.language.postfixOps

object EntryLinesParser extends RegexParsers {

  override def skipWhitespace = false
  override val whiteSpace = """[ \t]+""".r

  def ws = opt(whiteSpace)
  def newLine = namedError((("\r"?) ~> "\n"), "End of line expected")

  val entryName: Parser[String] = namedError("""[a-zA-Z][a-zA-Z0-9_.]*""".r, "illegal entry name")

  val treeKey: Parser[String] = '<' ~> ws ~> """[a-zA-Z0-9_.?*]*""".r <~ ws <~ '>'

  val mapKey: Parser[String] = '(' ~> ws ~> """[^)]*""".r <~ ws <~ ')'

  val entryId: Parser[EntryLineId] =
    entryName ~ treeKey ^^ { case name ~ key => TreeEntryLineId(name, key) } |
      entryName ~ mapKey ^^ { case name ~ key => MapEntryLineId(name, key) } |
      entryName ^^ { case name => SimpleEntryLineId(name) }

  /**
   *
   */
  val messageFormat: Parser[MessageFormat] = namedError(
    rep(
      """\\""" ^^ (_ => """\""")                          | // Handle escaped \\
        """\""" ~> ws ~> ("\r"?) ~> "\n" ~> ws ^^ (_ => "") | // Ignore escaped end of lines and following whitespace
        """\n""" ^^ (_ => "\n")                             | // Translate literal \n to real newline
        """.""".r                                             // keep any character (except new line)
    ) ^^ { case frags => new MessageFormat(frags.mkString.trim) },
    "message format expected"
  )

  val entryValue: Parser[EntryLineValue] =
    "->" ~> ws ~> entryName ^^ { case n => LinkEntryLineValue(n) } |
      '@' ~> ws ~> messageFormat ^^ { case m => MsgEntryLineValue(m, true) } |
      '=' ~> ws ~> messageFormat ^^ { case m => MsgEntryLineValue(m, false) }

  val entry: Parser[EntryLine] = (ws ~> entryId) ~ (ws ~> entryValue) ^^ {
    case k ~ v => EntryLine(k, v)
  }

  val comment: Parser[Option[EntryLine]] = """#.*""".r ^^ { case s => None }

  val line: Parser[Option[EntryLine]] = entry ^^ { case e => Some(e) } | comment | ws ^^ { _ => None }

  val lines: Parser[List[EntryLine]] = repsep(line, newLine) ^^ { case ls => ls collect { case Some(re) => re } }

  def namedError[A](p: Parser[A], msg: String) = Parser[A] { i =>
    p(i) match {
      case Failure(_, in) => Failure(msg, in)
      case o => o
    }
  }

  val phraseLines: Parser[List[EntryLine]] = phrase(lines)

  /**
   * Apply a parser to a given input and map the result to an Either.
   *
   * @param in
   * @param parser
   * @param ev
   * @tparam I
   * @tparam R
   * @return
   */
  def parse[I, R](in: I, parser: Parser[R])(implicit ev: Inputable[I]): Either[ev.ErrorInfo, R] = {
    ev.withInput(in, parser(_) match {
      case Success(r, _) => Right(r)
      case NoSuccess(message, in) => Left(message, in)
    })
  }

  def parsePhraseLines[I](in: I)(implicit ev: Inputable[I]): Either[ev.ErrorInfo, List[EntryLine]] = {
    parse(in, phraseLines)
  }

  trait Inputable[I] {

    type ErrorInfo = (String, Input, I)
    type Result[R] = Either[(String, Input), R]

    def withInput[R](i: I, f: Input => Result[R]): Either[ErrorInfo, R]

    protected def process[R](i: I, f: Input => Result[R], input: Input): Either[ErrorInfo, R] = {
      f(input) match {
        case Left(l) => Left((l._1, l._2, i))
        case Right(r) => Right(r)
      }
    }
  }

  object Inputable {
    implicit val UrlInputable = new Inputable[URL] {
      override def withInput[R](i: URL, f: Input => Result[R]): Either[ErrorInfo, R] = {
        val source = Source.fromURL(i, "UTF-8")
        try {
          process(i, f, StreamReader(source.reader()))
        } finally {
          source.close()
        }
      }
    }
    implicit val StringInputable = new Inputable[String] {
      override def withInput[R](i: String, f: Input => Result[R]): Either[ErrorInfo, R] = {
        process(i, f, new CharSequenceReader(i))
      }
    }
    implicit val InputStreamInputable = new Inputable[InputStream] {
      override def withInput[R](i: InputStream, f: Input => Result[R]): Either[ErrorInfo, R] = {
        val source = Source.fromInputStream(i, "UTF-8")
        try {
          process(i, f, StreamReader(source.reader()))
        } finally {
          source.close()
        }
      }
    }
  }

}

