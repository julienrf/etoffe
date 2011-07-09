package etoffe

trait Etoffe {
  /**
   * @param text Text to compile
   * @param indexGen Strategy to generate footnotes ids
   * @return AST of the compiled text
   */
  def parse(text: String, indexGen: IndexGenerator): Document
  
  /**
   * @param text Text to render
   * @param indexGen Strategy to generate footnotes ids
   * @return HTML code of the whole text, including footnotes
   */
  def render(text: String, indexGen: IndexGenerator): String
  
  /**
   * @param text Text to render
   * @param indexGen Strategy to generate footnotes ids
   * @return (paragraphs, footnotes): The list of HTML paragraphs and footnotes
   */
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
