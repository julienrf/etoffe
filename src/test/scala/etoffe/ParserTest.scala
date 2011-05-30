package etoffe

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

class ParserTest extends FunSuite with ShouldMatchers {
  
  test ("Empty input should produce an empty parse result") {
    Etoffe.parse("") should be (Document(List.empty))
  }
  
  test ("A single word without markup should produce one text block") {
    Etoffe.parse("word") should be (Document(List(Paragraph(List(Text("word"))))))
  }
  
  test ("New lines separate paragraphs") {
    Etoffe.parse("""|line1
                    |
                    |line2""".stripMargin) should be (Document(List(Paragraph(List(Text("line1"))),
                                                                    Paragraph(List(Text("line2"))))))
    Etoffe.parse("""|line1
                    |
                    |
                    |line2""".stripMargin) should be (Document(List(Paragraph(List(Text("line1"))),
                                                                    Paragraph(List(Text("line2"))))))
    Etoffe.parse("""|line1
                    |line2""".stripMargin) should be (Document(List(Paragraph(List(Text("line1"))),
                                                                    Paragraph(List(Text("line2"))))))
  }
  
  test ("Section") {
    Etoffe.parse("# foo") should be (Document(List(Section("foo"))))
    Etoffe.parse("""|# foo
                    |bar""".stripMargin) should be (Document(List(Section("foo"), Paragraph(List(Text("bar"))))))
    Etoffe.parse("""|# foo
                    |
                    |bar""".stripMargin) should be (Document(List(Section("foo"), Paragraph(List(Text("bar"))))))
  }
  
  test ("Bullets") {
    Etoffe.parse("""| - foo
                    | - bar""".stripMargin) should be (Document(List(BulletItem(Paragraph(List(Text("foo")))), BulletItem(Paragraph(List(Text("bar")))))))
  }
  
  test ("Text surrounded by stars is strongly emphasized") {
    Etoffe.parse("*foo*") should be (Document(List(Paragraph(List(Strong("foo"))))))
    Etoffe.parse("*foo*bar*") should be (Document(List(Paragraph(List(Strong("foo*bar"))))))
    Etoffe.parse("*foo bar*") should be (Document(List(Paragraph(List(Strong("foo bar"))))))
    Etoffe.parse("*foo") should be (Document(List(Paragraph(List(Text("*foo"))))))
    Etoffe.parse("foo*") should be (Document(List(Paragraph(List(Text("foo*"))))))
    Etoffe.parse("3*4*5") should be (Document(List(Paragraph(List(Text("3*4*5"))))))
    Etoffe.parse("3 * 4 * 5") should be (Document(List(Paragraph(List(Text("3"), Text("*"), Text("4"), Text("*"),  Text("5"))))))
  }
  
  test ("Text surrounded by underscores is emphasized") {
    Etoffe.parse("_foo_") should be (Document(List(Paragraph(List(Emphasized("foo"))))))
    Etoffe.parse("_foo_bar_") should be (Document(List(Paragraph(List(Emphasized("foo_bar"))))))
    Etoffe.parse("_foo bar_") should be (Document(List(Paragraph(List(Emphasized("foo bar"))))))
    Etoffe.parse("_foo") should be (Document(List(Paragraph(List(Text("_foo"))))))
    Etoffe.parse("foo_") should be (Document(List(Paragraph(List(Text("foo_"))))))
    Etoffe.parse("_foo_  bar") should be (Document(List(Paragraph(List(Emphasized("foo"), Text("bar"))))))
    Etoffe.parse("""|bar _foo_
                    |baz""".stripMargin) should be (Document(List(Paragraph(List(Text("bar"), Emphasized("foo"), Text("baz"))))))
  }
  
  test ("Special characters are perfectly handled") {
    Etoffe.parse("€") should be (Document(List(Paragraph(List(Text("€"))))))
    Etoffe.parse("*10 €*") should be (Document(List(Paragraph(List(Strong("10 €"))))))
    Etoffe.parse("_Play!_") should be (Document(List(Paragraph(List(Emphasized("Play!"))))))
    Etoffe.parse("Ça *m’intéresse*") should be (Document(List(Paragraph(List(Text("Ça"), Strong("m’intéresse"))))))
  }
  
  test ("Text surrounded by back ticks is code") {
    Etoffe.parse("`foo`") should be (Document(List(Paragraph(List(Code("foo"))))))
  }
  
  test ("Link") {
    Etoffe.parse(""""foo":/bar""") should be (Document(List(Paragraph(List(Link("foo", "/bar"))))))
    Etoffe.parse(""""foo":/bar#baz""") should be (Document(List(Paragraph(List(Link("foo", "/bar#baz"))))))
    Etoffe.parse(""""google":http://google.com""") should be (Document(List(Paragraph(List(Link("google", "http://google.com"))))))
    Etoffe.parse(""""google link":http://google.com""") should be (Document(List(Paragraph(List(Link("google link", "http://google.com"))))))
    // TODO url encoding
  }
  
  test ("Automatic link recognition") (pending) /*{
    Etoffe.parse("http://foo.com") should be (Document(List(Paragraph(List(Link("http://foo.com", "http://foo.com"))))))
    Etoffe.parse("http://foo.com/bar#baz") should be (Document(List(Paragraph(List(Link("http://foo.com/bar#baz", "http://foo.com/bar#baz"))))))
  }*/
  
  test ("Footnotes") (pending)
}
