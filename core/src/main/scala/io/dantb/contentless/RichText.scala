/*
 * Copyright 2023 Daniel Tattan-Birch
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package io.dantb.contentless

enum RichTextNodeType(val asString: String):
  case Heading1        extends RichTextNodeType("heading-1")
  case Heading2        extends RichTextNodeType("heading-2")
  case Heading3        extends RichTextNodeType("heading-3")
  case Heading4        extends RichTextNodeType("heading-4")
  case Heading5        extends RichTextNodeType("heading-5")
  case Heading6        extends RichTextNodeType("heading-6")
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
  def from(str: String): Option[RichTextNodeType] = RichTextNodeType.values.find(_.asString == str)

object RichText:

  sealed trait Node

  sealed trait Block extends Node:
    def content: List[Node]

  sealed trait Inline extends Node:
    def content: List[Node]

  case class Text(value: String, marks: List[Mark]) extends Node

  case class Document(content: List[Node]) extends Node

  enum Mark(val asString: String):
    case Bold        extends Mark("bold")
    case Italic      extends Mark("italic")
    case Underline   extends Mark("underline")
    case Code        extends Mark("code")
    case Subscript   extends Mark("subscript")
    case Superscript extends Mark("superscript")
  object Mark:
    def from(str: String): Option[Mark] = Mark.values.find(_.asString == str)

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
