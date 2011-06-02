package etoffe

import collection.mutable.{Buffer, ListBuffer}
import index.{Generator, NumberGenerator}

trait HtmlRenderer {
  
  def render(document: Document, indexGen: Generator = new NumberGenerator): String = {
    val blocks = ListBuffer.empty[String]
    val footnotes = ListBuffer.empty[String]
    
    val itBlock = document.blocks.iterator
    while (itBlock.hasNext) {
      val buffer = new StringBuilder
      itBlock.next match {
        case Section(title) => {
          buffer ++= "<h2>"
          escapeAndAppend(title, buffer)
          buffer ++= "</h2>"
        }
        case Bullet(paragraph) => {
          buffer ++= "<ul><li>"
          appendParagraph(paragraph, buffer, footnotes, indexGen)
          buffer ++= "</li></ul>"
        }
        case p: Paragraph => {
          buffer ++= "<p>"
          appendParagraph(p, buffer, footnotes, indexGen)
          buffer ++= "</p>"
        }
      }
      if (itBlock.hasNext) {
        buffer ++= "\n"
      }
      blocks += buffer.toString
    }
    
    blocks.mkString + footnotes.mkString
  }
  
  private def escapeAndAppend(content: String, buffer: StringBuilder) {
    for (char <- content) {
      char match {
        case '<' => buffer ++= "&lt;"
        case '>' => buffer ++= "&gt;"
        case '&' => buffer ++= "&amp;"
        case '"' => buffer ++= "&quot;"
        case c => buffer += c
      }
    }
  }
  
  private def wrapAndAppend(content: String, tag: String, buffer: StringBuilder) {
    // FIXME is it _really_ faster than string concatenation?
    buffer += '<'
    buffer ++= tag
    buffer += '>'
    escapeAndAppend(content, buffer)
    buffer ++= "</"
    buffer ++= tag
    buffer += '>'
  }
  
  private def appendParagraph(paragraph: Paragraph, buffer: StringBuilder, footnotes: Buffer[String], indexGen: Generator) {
    val itInline = paragraph.content.iterator
    while (itInline.hasNext) {
      itInline.next match {
        case Text(word) => escapeAndAppend(word, buffer)
        case Emphasized(content) => wrapAndAppend(content, "em", buffer)
        case Strong(content) => wrapAndAppend(content, "strong", buffer)
        case Code(content) => wrapAndAppend(content, "code", buffer)
        case Link(title, url) => {
          buffer ++= "<a href=\""
          escapeAndAppend(url, buffer) // FIXME urlencode?
          buffer ++= "\" title=\""
          escapeAndAppend(title, buffer)
          buffer ++= "\">"
          escapeAndAppend(title, buffer)
          buffer ++= "</a>"
        }
        case Footnote(content) => {
          val index = indexGen.next()
          buffer ++= "<sup>[<a href=\"#footnote_"
          buffer ++= index
          buffer ++= "\">"
          buffer ++= index
          buffer ++= "</a>]</sup>"
          val footnote = new StringBuilder
          footnote ++= "<p id=\"footnote_"
          footnote ++= index
          footnote ++= "\">["
          footnote ++= index
          footnote ++= "] "
          footnote ++= content
          footnote ++= "</p>"
          footnotes += footnote.toString
        }
      }
      if (itInline.hasNext) {
        buffer.append(" ")
      }
    }
  }
  
}

object HtmlRenderer extends HtmlRenderer
