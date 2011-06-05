package etoffe

import collection.mutable.{Buffer, ListBuffer}

trait HtmlRenderer {
  
  def render(document: Document, indexGen: IndexGenerator = new NumberGenerator): (Traversable[String], Traversable[String]) = {
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
    
    (blocks, footnotes)
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
  
  private def appendParagraph(paragraph: Paragraph, buffer: StringBuilder, footnotes: Buffer[String], indexGen: IndexGenerator) {
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
          val footnoteStr = new StringBuilder
          footnoteStr ++= "<p id=\"footnote_"
          footnoteStr ++= index
          footnoteStr ++= "\">["
          footnoteStr ++= index
          footnoteStr ++= "] "
          escapeAndAppend(content, footnoteStr)
          footnoteStr ++= "</p>"
          footnotes += footnoteStr.toString
        }
      }
      if (itInline.hasNext) {
        buffer.append(" ")
      }
    }
  }
  
}

object HtmlRenderer extends HtmlRenderer
