package etoffe.java

import etoffe._
import _root_.java.lang.{Iterable => JIterable}
import collection.JavaConversions

/**
 * Convenient object to help poor Java programmers to use Etoffe
 */
object JEtoffe {
  def parse(text: String, indexGen: IndexGenerator) = Etoffe.parse(text, indexGen)
  
  def render(text: String, indexGen: IndexGenerator) = Etoffe.render(text, indexGen)
  
  def renderParagraphs(text: String, indexGen: IndexGenerator): EtoffeResult = {
    val (paragraphs: JIterable[String], footnotes: JIterable[String]) = Etoffe.renderParagraphs(text, indexGen)
    new EtoffeResult(paragraphs, footnotes)
  }
}

class EtoffeResult(val paragraphs: JIterable[String], val footnotes: JIterable[String])