package etoffe

trait Etoffe {
  /**
   * @param text Text to compile
   * @return Html paragraphs corresponding to source text, and the footnotes
   */
  def parse(text: String, indexGen: IndexGenerator): Document
  
  def render(text: String, indexGen: IndexGenerator): String
  
  def renderParagraphs(text: String, indexGen: IndexGenerator): (Traversable[String], Traversable[String])
}

object Etoffe extends Etoffe {
  
  override def parse(text: String, indexGen: IndexGenerator = new NumberGenerator): Document =
    Parser.parse(text)
  
  override def render(text: String, indexGen: IndexGenerator = new NumberGenerator): String = {
    val (blocks, footnotes) = HtmlRenderer.render(parse(text))
    blocks.mkString + footnotes.mkString
  }
  
  override def renderParagraphs(text: String, indexGen: IndexGenerator = new NumberGenerator): (Traversable[String], Traversable[String]) = {
    HtmlRenderer.render(parse(text))
  }
}
