package etoffe

trait Etoffe {
  /**
   * @param text Text to compile
   * @return Html paragraphs corresponding to source text, and the footnotes
   */
  def parse(text: String): Document
  
  def render(text: String): String
}

object Etoffe extends Etoffe {
  override def parse(text: String): Document = Parser.parse(text)
  
  override def render(text: String): String = {
    HtmlRenderer.render(parse(text))
  }
}
