package io.dantb.contentless

object RichText {

  sealed trait Node

  sealed trait Block extends Node {
    def content: List[Node]
  }

  sealed trait Inline extends Node {
    def content: List[Node]
  }

  case class Text(value: String, marks: List[Mark]) extends Node

  case class Document(content: List[Node]) extends Node

  sealed trait Mark {
    val `type`: String = this match {
      case Mark.Bold      => "bold"
      case Mark.Italic    => "italic"
      case Mark.Underline => "underline"
      case Mark.Code      => "code"
    }
  }

  object Mark {
    object Bold      extends Mark
    object Italic    extends Mark
    object Underline extends Mark
    object Code      extends Mark

    val values: List[Mark]              = List(Bold, Italic, Underline, Code)
    def from(str: String): Option[Mark] = values.find(_.`type` == str)
  }

  case class Heading1(content: List[Node])   extends Block
  case class Heading2(content: List[Node])   extends Block
  case class Heading3(content: List[Node])   extends Block
  case class Heading4(content: List[Node])   extends Block
  case class Heading5(content: List[Node])   extends Block
  case class Heading6(content: List[Node])   extends Block
  case class Paragraph(content: List[Node])  extends Block
  case class Quote(content: List[Paragraph]) extends Block

  /** Horizontal rule */
  case class Hr(content: List[Node]) extends Block

  /** OL */
  case class OrderedList(content: List[ListItem]) extends Block

  /** UL */
  case class UnorderedList(content: List[ListItem]) extends Block

  /** LI */
  case class ListItem(content: List[Node]) extends Block

  case class EntryLinkBlock(content: List[Node], reference: Reference) extends Block
  case class AssetLinkBlock(content: List[Node], reference: Reference) extends Block

  case class EntryLinkInline(content: List[Text], reference: Reference) extends Inline
  case class Hyperlink(content: List[Text], uri: String)                extends Inline
  case class AssetHyperlink(content: List[Text], reference: Reference)  extends Inline
  case class EntryHyperlink(content: List[Text], reference: Reference)  extends Inline

}
