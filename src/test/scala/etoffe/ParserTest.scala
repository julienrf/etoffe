package etoffe

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

class ParserTest extends FunSuite with ShouldMatchers {
  
  test ("word parser") {
    Parser.parse(Parser.word, "foo").get should be (Text("foo"))
    Parser.parse(Parser.word, "foo-bar").get should be (Text("foo"))
    Parser.parse(Parser.word, "foo_bar").get should be (Text("foo"))
    Parser.parse(Parser.word, "42").get should be (Text("42"))
    Parser.parse(Parser.word, "éçà").get should be (Text("éçà"))
    Parser.parse(Parser.word, ".") should not be ('successful)
  }
  
  test ("punct parser") {
    Parser.parse(Parser.punct, ",").get should be (Text(","))
    Parser.parse(Parser.punct, "ç") should not be ('successful)
    Parser.parse(Parser.punct, "5") should not be ('successful)
  }
  
  test ("space and words or punct parser") {
    Parser.parse(Parser.spacesAndWordsOrPuncts(Parser.wordOrPunct("*")), "foo bar, baz").get should be (Text("foo bar, baz"))
    Parser.parse(Parser.spacesAndWordsOrPuncts(Parser.wordOrPunct), "foo bar, baz").get should be (Text("foo bar, baz"))
  }
  
  test ("text parser") {
    Parser.parse(Parser.text, "Hello World!").get should be (Text("Hello World!"))
    //Parser.parse(Parser.text, " foo") should not be ('successful)
    Parser.parse(Parser.text, "foo bar, baz").get should be (Text("foo bar, baz"))
  }
  
  test ("strong parser") {
    Parser.parse(Parser.strong, "*Hello*").get should be (Strong("Hello"))
    Parser.parse(Parser.strong, "*Hello World*").get should be (Strong("Hello World"))
    Parser.parse(Parser.strong, "*Foo.*").get should be (Strong("Foo."))
    //Parser.parse(Parser.strong, "1*2*3").get should be (Text("1*2*3"))
    Parser.parse(Parser.strong, "*") should not be ('successful)
  }
  
  test ("inline parser") {
    Parser.parse(Parser.inline, "Hello World!").get should be (Text("Hello World!"))
    Parser.parse(Parser.inline, "*Hello World!*").get should be (Strong("Hello World!"))
    Parser.parse(Parser.inline, "_Hello World!_").get should be (Emphasized("Hello World!"))
    Parser.parse(Parser.inline, "Hello World!*").get should be (Text("Hello World!"))
  }
  
  test ("paragraph") {
    Parser.parse(Parser.paragraph, "Foo").get should be (Paragraph(List(Text("Foo"))))
  }
  
  
  test ("empty input should produce an empty parse result") {
    Etoffe.parse("") should be (Document(List.empty))
  }
  
  test ("a single word without markup should produce one text block") {
    Etoffe.parse("word") should be (Document(List(Paragraph(List(Text("word"))))))
  }
  
  test ("new lines separate paragraphs") {
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
  
  test ("section") {
    Etoffe.parse("# foo") should be (Document(List(Section("foo"))))
    Etoffe.parse("""|# foo
                    |bar""".stripMargin) should be (Document(List(Section("foo"), Paragraph(List(Text("bar"))))))
    Etoffe.parse("""|# foo
                    |
                    |bar""".stripMargin) should be (Document(List(Section("foo"), Paragraph(List(Text("bar"))))))
  }
  
  test ("bullets") {
    Etoffe.parse("""| - foo
                    | - bar""".stripMargin) should be (Document(List(Bullet(Paragraph(List(Text("foo")))), Bullet(Paragraph(List(Text("bar")))))))
  }
  
  test ("text surrounded by stars is strongly emphasized") {
    Etoffe.parse("*foo*") should be (Document(List(Paragraph(List(Strong("foo"))))))
    Etoffe.parse("*foo*bar*") should be (Document(List(Paragraph(List(Strong("foo*bar"))))))
    Etoffe.parse("*foo bar*") should be (Document(List(Paragraph(List(Strong("foo bar"))))))
    Etoffe.parse("*foo") should be (Document(List(Paragraph(List(Text("*foo"))))))
    Etoffe.parse("foo*") should be (Document(List(Paragraph(List(Text("foo"), Text("*"))))))
    Etoffe.parse("3*4*5") should be (Document(List(Paragraph(List(Text("3"), Text("*4"), Text("*5"))))))
    Etoffe.parse("3 * 4 * 5") should be (Document(List(Paragraph(List(Text("3 "), Text("* 4 "), Text("* 5"))))))
  }
  
  test ("text surrounded by underscores is emphasized") {
    Etoffe.parse("_foo_") should be (Document(List(Paragraph(List(Emphasized("foo"))))))
    Etoffe.parse("_foo_bar_") should be (Document(List(Paragraph(List(Emphasized("foo_bar"))))))
    Etoffe.parse("_foo bar_") should be (Document(List(Paragraph(List(Emphasized("foo bar"))))))
    Etoffe.parse("_foo") should be (Document(List(Paragraph(List(Text("_foo"))))))
    Etoffe.parse("foo_") should be (Document(List(Paragraph(List(Text("foo"), Text("_"))))))
    Etoffe.parse("_foo_  bar") should be (Document(List(Paragraph(List(Emphasized("foo"), Text("  bar"))))))
    Etoffe.parse("_foo_,  bar") should be (Document(List(Paragraph(List(Emphasized("foo"), Text(",  bar"))))))
    Etoffe.parse("""|bar _foo_
                    |baz""".stripMargin) should be (Document(List(Paragraph(List(Text("bar "), Emphasized("foo"))), Paragraph(List(Text("baz"))))))
  }
  
  test ("special characters are perfectly handled") {
    Etoffe.parse("€") should be (Document(List(Paragraph(List(Text("€"))))))
    Etoffe.parse("*10 €*") should be (Document(List(Paragraph(List(Strong("10 €"))))))
    Etoffe.parse("_Play!_") should be (Document(List(Paragraph(List(Emphasized("Play!"))))))
    Etoffe.parse("Ça *m’intéresse*") should be (Document(List(Paragraph(List(Text("Ça "), Strong("m’intéresse"))))))
  }
  
  test ("text surrounded by back ticks is code") {
    Etoffe.parse("`foo`") should be (Document(List(Paragraph(List(Code("foo"))))))
  }
  
  test ("link") {
    Etoffe.parse(""""foo":/bar""") should be (Document(List(Paragraph(List(Link("foo", "/bar"))))))
    Etoffe.parse(""""foo":/bar#baz""") should be (Document(List(Paragraph(List(Link("foo", "/bar#baz"))))))
    Etoffe.parse(""""google":http://google.com""") should be (Document(List(Paragraph(List(Link("google", "http://google.com"))))))
    Etoffe.parse(""""google link":http://google.com""") should be (Document(List(Paragraph(List(Link("google link", "http://google.com"))))))
    // TODO url encoding
  }
  
  test ("automatic link recognition") (pending) /*{
    Etoffe.parse("http://foo.com") should be (Document(List(Paragraph(List(Link("http://foo.com", "http://foo.com"))))))
    Etoffe.parse("http://foo.com/bar#baz") should be (Document(List(Paragraph(List(Link("http://foo.com/bar#baz", "http://foo.com/bar#baz"))))))
  }*/
  
  test ("footnotes") {
    Etoffe.parse("foo [footnote] bar") should be (Document(List(Paragraph(List(Text("foo "), Footnote("footnote"), Text(" bar"))))))
    Etoffe.parse("foo [one two]") should be (Document(List(Paragraph(List(Text("foo "), Footnote("one two"))))))
  }
  
  test ("non ASCII characters") {
    Etoffe.parse("ç«»ué") should be (Document(List(Paragraph(List(Text("ç«»ué"))))))
  }
}
