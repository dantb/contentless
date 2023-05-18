package io.dantb.contentless

enum RichTextNodeType(val asString: String):
  case Heading1        extends RichTextNodeType("heading-1")
  case Heading2        extends RichTextNodeType("heading-2")
  case Heading3        extends RichTextNodeType("heading-3")
  case Heading4        extends RichTextNodeType("heading-4")
  case Heading5        extends RichTextNodeType("heading-5")
  case Heading6        extends RichTextNodeType("heading-6")
  case Paragraph       extends RichTextNodeType("paragraph")
  case Quote           extends RichTextNodeType("blockquote")
  case Hr              extends RichTextNodeType("hr")
  case OrderedList     extends RichTextNodeType("ordered-list")
  case UnorderedList   extends RichTextNodeType("unordered-list")
  case Hyperlink       extends RichTextNodeType("hyperlink")
  case EntryLinkInline extends RichTextNodeType("embedded-entry-inline")
  case EntryHyperlink  extends RichTextNodeType("entry-hyperlink")
  case EntryLinkBlock  extends RichTextNodeType("embedded-entry-block")
  case AssetHyperlink  extends RichTextNodeType("asset-hyperlink")
  case AssetLinkBlock  extends RichTextNodeType("embedded-asset-block")
  case Table           extends RichTextNodeType("table")

object RichTextNodeType:
  import RichTextNodeType.*
  val All: Set[RichTextNodeType] = Set(
    Heading1,
    Heading2,
    Heading3,
    Heading4,
    Heading5,
    Heading6,
    Paragraph,
    Quote,
    Hr,
    OrderedList,
    UnorderedList,
    Hyperlink,
    EntryLinkInline,
    EntryHyperlink,
    EntryLinkBlock,
    AssetHyperlink,
    AssetLinkBlock,
    Table
  )
  def from(str: String): Option[RichTextNodeType] = All.find(_.asString == str)

object RichText:

  sealed trait Node

  sealed trait Block extends Node:
    def content: List[Node]

  sealed trait Inline extends Node:
    def content: List[Node]

  case class Text(value: String, marks: List[Mark]) extends Node

  case class Document(content: List[Node]) extends Node

  enum Mark(val asString: String):
    case Bold      extends Mark("bold")
    case Italic    extends Mark("italic")
    case Underline extends Mark("underline")
    case Code      extends Mark("code")
  object Mark:
    val All: Set[Mark]                  = Set(Mark.Bold, Mark.Italic, Mark.Underline, Mark.Code)
    def from(str: String): Option[Mark] = All.find(_.asString == str)

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
