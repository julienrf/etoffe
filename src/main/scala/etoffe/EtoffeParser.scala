package etoffe

import util.parsing.combinator.RegexParsers

trait EtoffeParser extends RegexParsers {
  
  def document: Parser[Document] = repsep(block, "(\r?\n)+".r) ^^ { blocks => Document(blocks) }
  
  
  def block: Parser[Block] = section | paragraph
  
  def section: Parser[Section] = "# " ~> ".*".r ^^ { s => Section(s) }
  
  def paragraph: Parser[Paragraph] = ((spaces ~> inline) +) ^^ { inlines => Paragraph(inlines) }
  
  
  def inline: Parser[Inline] = strong | emphasis | code | link | footnote | word
  
  def word: Parser[Text] = WordPattern ^^ { case WordPattern(word, _) => Text(word) }
  val WordPattern = """([^\s]+)(\s|\z)""".r
  
  
  def strong: Parser[Strong] = StrongPattern ^^ { case StrongPattern(content, _) => Strong(content) }
  val StrongPattern = inlinePattern(delim = """\*""")
  
  def emphasis: Parser[Emphasized] = EmPattern ^^ { case EmPattern(content, _) => Emphasized(content) }
  val EmPattern = inlinePattern(delim = "_")
  
  def code: Parser[Code] = CodePattern ^^ { case CodePattern(content, _) => Code(content) }
  val CodePattern = inlinePattern(delim ="`")
  
  def link: Parser[Link] = LinkPattern ^^ { case LinkPattern(title, url, _) => Link(title, url) }
  val LinkPattern = """"([^\s].*?)":([^\s].*?)(\s|\z)""".r
  
  def footnote: Parser[FootnoteRef] = failure("TODO")
  
  def inlinePattern(delim: String) = (delim + """([^\s].*?)""" + delim + """(\s|\z)""").r
  val spaces = """\p{Blank}*""".r
  
  override val whiteSpace = "".r
}

object EtoffeParser extends EtoffeParser {
  def parse(text: String): Document = {
    parse(document, text).getOrElse(Document.empty)
  }
}
