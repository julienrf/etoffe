package etoffe

import util.parsing.combinator.RegexParsers
import util.matching.Regex

trait Parser extends RegexParsers {
  
  override val whiteSpace = "".r
  
  // A word is just a sequence of letters or digitss
  val word = """[\p{L}\p{Nd}]+""".r ^^ { w => Text(w) }
  
  val spaces = """[ \t]+""".r ^^ { s => Text(" ") }
  
  val newline = """\r?\n""".r
  val newlines = newline*

  val EOI = """\z""".r
  
  // Anything but a word, a space or a newline
  val punct = """[^\p{L}\p{Nd} \t\r\n]""".r ^^ { p => Text(p) }
  // Any punctuation mark but except
  def punct(except: String): Parser[Text] = not(except) ~> punct
  
  // Consume content until the potential start of a markup or the end of the line
  val text: Parser[Text] = ("""[^\r\n]""".r ~ (("""[^*_"`\[\r\n]""".r)*)) ^^ { case c ~ cs => Text(c + cs.mkString) }
  // Word or punctuation mark. If the punctuation mark is sep, make sure it is followed by a word
  val wordOrPunct: Parser[Text] = word | punct
  def wordOrPunct(sep: String): Parser[Text] = (word | ((sep <~ guard(word)) ^^^ { Text(sep) }) | punct(sep))
  // (spaces? (wordOrPunct))*
  def spacesAndWordsOrPuncts(wordOrPunct: Parser[Text]) = ((((spaces?) ~ wordOrPunct) ^^ { case Some(s) ~ pw => Text(s.text + pw.text) ; case None ~ pw => pw })*) ^^ { ws => ws.foldLeft(Text(""))((w1, w2) => Text(w1.text + w2.text)) }
  
  def inline(start: String, stop: String): Parser[Text] = (start ~> (word ~ spacesAndWordsOrPuncts(wordOrPunct(stop))) <~ stop) ^^ { case w1 ~ w2 => Text(w1.text + w2.text) }
  def inline(delim: String): Parser[Text] = inline(delim, delim)
  
  val strong: Parser[Strong] = inline("*") ^^ { t => Strong(t.text) }
  val emphasis: Parser[Emphasized] = inline("_") ^^ { t => Emphasized(t.text) }
  val code: Parser[Code] = inline("`") ^^ { t => Code(t.text) }
  
  val LinkPattern = """"(\S.*?)":(\S+[\w/])""".r
  val link: Parser[Link] = LinkPattern ^^ { case LinkPattern(title, url) => Link(title, url) }
  
  val footnote: Parser[Footnote] = inline("[", "]") ^^ { t => Footnote(t.text) }
  
  // Inline content
  val inline: Parser[Inline] = strong | emphasis | code | link | footnote | text
  
  val paragraph: Parser[Paragraph] = ((inline+) <~ (newline | EOI)) ^^ { inlines => Paragraph(inlines) }
  val bullets: Parser[Bullet] = " *- ".r ~> paragraph ^^ { p => Bullet(p) }
  val section: Parser[Section] = "# " ~> ".+".r ^^ { s => Section(s) }
  
  val block: Parser[Block] = section | bullets | paragraph
  
  val document: Parser[Document] = rep(newlines ~> block) ^^ { blocks => Document(blocks) }
}

object Parser extends Parser {
  def parse(text: String): Document = {
    parse(document, text).getOrElse(Document.empty)
  }
}
