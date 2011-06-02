package etoffe

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

class HtmlRendererTest extends FunSuite with ShouldMatchers {
  test ("Empty") {
    Etoffe.render("") should be ("")
  }
  
  test ("Basic text") {
    Etoffe.render("""|# A Section
                     |
                     |Paragraph content with _emphasized_ and *strong* content.
                     |
                     |Another paragraph with a "link":http://google.fr""".stripMargin) should be
    ("""|<h2>A Section</h2>
        |<p>Paragraph content with <em>emphasized</em> and <strong>strong</strong> content.</p>
        |<p>Another paragraph with a <a href="http://google.fr" title="link">link</a></p>""".stripMargin)
  }
  
  test ("Html escape") {
    Etoffe.render("<hr />") should be ("<p>&lt;hr /&gt;</p>")
  }
  
  test ("Footnotes") {
    Etoffe.render("See [Programming in Scala, M. Odersky]") should be
      ("""|<p>See <sup>[<a href="#footnote_1">1</a>]</sup></p>
          |<p id="footnote_1">[1] Programming in Scala, M. Odersky</p>""")
  }
}
