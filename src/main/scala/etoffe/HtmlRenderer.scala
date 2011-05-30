package etoffe

trait HtmlRenderer {
  def render(document: Document): String = {
    val blocks = collection.mutable.ListBuffer.empty[String]
    val footnotes = collection.mutable.ListBuffer.empty[String]
    
    def escapeAndAppend(content: String, buffer: StringBuilder) {
      for (char <- content) {
        char match {
          case '<' => buffer.append("&lt;")
          case '>' => buffer.append("&gt;")
          case '&' => buffer.append("&amp;")
          case '"' => buffer.append("&quot;")
          case c => buffer.append(c)
        }
      }
    }
    
    def wrapAndAppend(content: String, tag: String, buffer: StringBuilder) {
      // FIXME is it _really_ faster than string concatenation?
      buffer.append("<")
      buffer.append(tag)
      buffer.append(">")
      escapeAndAppend(content, buffer)
      buffer.append("</")
      buffer.append(tag)
      buffer.append(">")
    }
    
    val itBlock = document.blocks.iterator
    while (itBlock.hasNext) {
      val buffer = new StringBuilder
      itBlock.next match {
        case Section(title) => {
          buffer.append("<h2>")
          escapeAndAppend(title, buffer)
          buffer.append("</h2>")
        }
        case Paragraph(inlines) => {
          buffer.append("<p>")
          val itInline = inlines.iterator
          while (itInline.hasNext) {
            itInline.next match {
              case Text(word) => escapeAndAppend(word, buffer)
              case Emphasized(content) => wrapAndAppend(content, "em", buffer)
              case Strong(content) => wrapAndAppend(content, "strong", buffer)
              case Code(content) => wrapAndAppend(content, "code", buffer)
              case Link(title, url) => {
                buffer.append("<a href=\"")
                escapeAndAppend(url, buffer) // FIXME urlencode?
                buffer.append("\" title=\"")
                escapeAndAppend(title, buffer)
                buffer.append("\">")
                escapeAndAppend(title, buffer)
                buffer.append("</a>")
              }
            }
            if (itInline.hasNext) {
              buffer.append(" ")
            }
          }
          buffer.append("</p>")
        }
      }
      if (itBlock.hasNext) {
        buffer.append("\n")
      }
      blocks += buffer.toString
    }
    
    blocks.mkString + footnotes.mkString
  }
}

object HtmlRenderer extends HtmlRenderer
