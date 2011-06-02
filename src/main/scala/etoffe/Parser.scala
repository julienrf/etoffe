package etoffe

import util.parsing.combinator.RegexParsers
import util.matching.Regex

trait Parser extends RegexParsers {
  
  def document: Parser[Document] = rep(newlines ~> block) ^^ { blocks => Document(blocks) }
  
  
  def block: Parser[Block] = section | bullets | paragraph
  
  def section: Parser[Section] = "# " ~> ".+".r ^^ { s => Section(s) }
  
  def bullets: Parser[Bullet] = " *- ".r ~> paragraph ^^ { p => Bullet(p) }
  
  def paragraph: Parser[Paragraph] = (((spaces ~> inline) +) <~ (newline | EOI)) ^^ { inlines => Paragraph(inlines) }
  
  
  def inline: Parser[Inline] = strong | emphasis | code | link | footnote | word
  
  def word: Parser[Text] = """\S+""".r ^^ { s => Text(s) }
  
  def strong: Parser[Strong] = StrongPattern ^^ { case StrongPattern(content, _) => Strong(content) }
  val StrongPattern = inlinePattern(delim = """\*""")
  
  def emphasis: Parser[Emphasized] = EmPattern ^^ { case EmPattern(content, _) => Emphasized(content) }
  val EmPattern = inlinePattern(delim = "_")
  
  def code: Parser[Code] = CodePattern ^^ { case CodePattern(content, _) => Code(content) }
  val CodePattern = inlinePattern(delim ="`")
  
  def link: Parser[Link] = LinkPattern ^^ { case LinkPattern(title, url, _) => Link(title, url) }
  val LinkPattern = """"(\S.*?)":(\S+?)(\s|\z)""".r
  
  def footnote: Parser[Footnote] = FootnotePattern ^^ { case FootnotePattern(content, _) => Footnote(content) }
  val FootnotePattern = inlinePattern("\\[", "\\]")
  
  def inlinePattern(delim: String): Regex = inlinePattern(delim, delim)
  def inlinePattern(start: String, stop: String) = (start + """(\S.+?)""" + stop + """(\s|\z)""").r
  val spaces = """[ \t]*""".r
  val newline = """\r?\n""".r
  val newlines = """(\r?\n)*""".r
  val EOI = """\z""".r
  
  override val whiteSpace = "".r
}

object Parser extends Parser {
  def parse(text: String): Document = {
    parse(document, text).getOrElse(Document.empty)
  }
}
