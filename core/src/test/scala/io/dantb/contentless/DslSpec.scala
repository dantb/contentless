package io.dantb.contentless.codecs

import java.time.ZonedDateTime

import cats.syntax.all.*
import io.dantb.contentless.*
import io.dantb.contentless.Validation.Size
import io.dantb.contentless.appearance.Control.DateTimeControl
import io.dantb.contentless.appearance.FieldControlSetting.DatePicker.ClockType
import io.dantb.contentless.dsl.*
import munit.ScalaCheckSuite
import org.scalacheck.Prop.*
import io.dantb.contentless.appearance.SidebarWidget

// Examples of user-facing API used as the basic for the documentation.
class DslSpec extends ScalaCheckSuite:

  enum Tag(val slug: String):
    case Sport      extends Tag("sport")
    case Politics   extends Tag("politics")
    case Science    extends Tag("science")
    case Technology extends Tag("technology")

  def allTags                             = Set(Tag.Sport, Tag.Politics, Tag.Science, Tag.Technology)
  def parseTag(slug: String): Option[Tag] = allTags.find(_.slug == slug)

  final case class Author(name: String, image: Media, bio: Option[String])

  object Author:
    val id: ContentTypeId = ContentTypeId("author")

    val name: EntryCodec[String]        = text("name", "Name").required
    val image: EntryCodec[Media]        = asset("image", "Image", Set(MimeTypeGroup.Image)).required
    val bio: EntryCodec[Option[String]] = longText("bio", "Biography").optional

    given authorCodec: EntryCodec[Author] = (name *: image *: bio).to[Author]

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

    val title: EntryCodec[String]       = text("title", "Title").required
    val body: EntryCodec[RichText.Node] = richText("body", "Body").required
    val tags: EntryCodec[Set[Tag]] =
      textList("tags", "Tags").required
        .eimap(_.traverse(t => parseTag(t).toRight(s"Invalid tag: $t")).map(_.toSet))(_.map(_.slug).toList)
    val authors: EntryCodec[Set[Reference]] =
      entries("authors", "Authors", Set(Author.id)).required.eimap(_.toSet.asRight)(_.toList)
    val coverImage: EntryCodec[Option[Media]]  = asset("coverImage", "Cover Image", Set(MimeTypeGroup.Image)).optional
    val displayDate: EntryCodec[ZonedDateTime] = zonedDateTime("displayDate", "Display Date").required

    val articleCodec: EntryCodec[Article] = (title *: body *: tags *: authors *: coverImage *: displayDate).to[Article]

    val articleCodecCustomisedEntryEditor = articleCodec.withSidebar(List(SidebarWidget.BuiltIn.InfoPanel, SidebarWidget.BuiltIn.Publication, SidebarWidget.BuiltIn.Users))
  object docs:
    object Author:
      val id: ContentTypeId = ContentTypeId("author")

      val codec: EntryCodec[Author] =
        (text("name", "Name").required *:
          asset("image", "Image", Set(MimeTypeGroup.Image)).required *:
          longText("bio", "Biography").optional).to[Author]

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
            arrayBounds = Some(Size(min = Some(1), max = Some(3), message = Some("Please select 1-3 tags")))
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

  test("WAT") {}
