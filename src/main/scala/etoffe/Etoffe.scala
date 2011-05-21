package etoffe

trait Etoffe {
  /**
   * @param text Text to compile
   * @return Html paragraphs corresponding to source text, and the footnotes
   */
  def parse(text: String): Document
}

object Etoffe extends Etoffe {
  override def parse(text: String): Document = EtoffeParser.parse(text)
}
