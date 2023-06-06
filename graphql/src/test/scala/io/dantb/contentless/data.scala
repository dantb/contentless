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

import java.time.ZonedDateTime

import cats.syntax.all.*
import io.dantb.contentless.*
import io.dantb.contentless.Validation.Size
import io.dantb.contentless.appearance.Control.DateTimeControl
import io.dantb.contentless.appearance.FieldControlSetting.DatePicker.ClockType
import io.dantb.contentless.appearance.SidebarWidget
import io.dantb.contentless.codecs.EntryCodec
import io.dantb.contentless.dsl.*

enum Tag(val slug: String):
  case Sport      extends Tag("sport")
  case Politics   extends Tag("politics")
  case Science    extends Tag("science")
  case Technology extends Tag("technology")

def parseTag(slug: String): Option[Tag] = Tag.values.find(_.slug == slug)

final case class Author(name: String, image: Media, bio: Option[String])

object Author:
  val id: ContentTypeId = ContentTypeId("author")

  val codec: EntryCodec[Author] =
    (text("name", "Name").required *:
      asset("image", "Image", Set(MimeTypeGroup.Image)).required *:
      longText("bio", "Biography").optional).to[Author]

  given ct: ContentType[Author] = contentType(id, "Author", "name".some, "Author of 'Article' content type".some, codec)

final case class Article(
    title: String,
    body: RichText.Node,
    tags: Set[Tag],
    authors: Set[Reference],
    coverImage: Option[Media],
    displayDate: ZonedDateTime
)

object Article:
  val id: ContentTypeId = ContentTypeId("article")

  val codec: EntryCodec[Article] =
    (text("title", "Title").required *:
      richText(
        "body",
        "Body",
        allowedNodeTypes = Set(RichTextNodeType.Heading1, RichTextNodeType.OrderedList)
      ).required *:
      textList(
        "tags",
        "Tags",
        defaultValue = Some(List(Tag.Technology.slug)),
        arrayBounds = Size.range(min = 1, max = 3, "Please select 1-3 tags").some
      ).required
        .eimap(_.traverse(t => parseTag(t).toRight(s"Invalid tag: $t")).map(_.toSet))(_.map(_.slug).toList) *:
      entries("authors", "Authors", Set(Author.id)).required.eimap(_.toSet.asRight)(_.toList) *:
      asset("coverImage", "Cover Image", Set(MimeTypeGroup.Image)).optional *:
      zonedDateTime(
        "displayDate",
        "Display Date",
        dateTimeControl = DateTimeControl.ZonedDefault
          .withClockType(ClockType.TwelveHour)
          .withHelpText("Enter zoned display date in 12H format")
      ).required).to[Article]

  val articleCodecCustomisedEntryEditor = codec.withSidebar(
    List(SidebarWidget.BuiltIn.InfoPanel, SidebarWidget.BuiltIn.Publication, SidebarWidget.BuiltIn.Users)
  )

  given ct: ContentType[Article] = contentType(id, "Article", "title".some, "Article with rich text body".some, codec)
