package etoffe


case class Document(blocks: List[Block])

object Document {
  val empty = Document(List.empty)
}


sealed abstract class Block

case class Paragraph(content: List[Inline]) extends Block

case class Section(title: String) extends Block

sealed abstract class Bullets(items: List[Bullets]) extends Block

case class BulletNode(items: List[Bullets]) extends Bullets(items)

case class BulletItem(content: Paragraph) extends Bullets(Nil)


sealed abstract class Inline

case class Text(text: String) extends Inline

case class Emphasized(text: String) extends Inline

case class Strong(text: String) extends Inline

case class StrongEmphasized(text: String) extends Inline

case class Code(text: String) extends Inline

case class Link(title: String, url: String) extends Inline

case class FootnoteRef(ref: Footnote) extends Inline


case class Footnote(content: Paragraph)
