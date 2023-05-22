package io.dantb.contentless.codecs

import java.time.{LocalDate, LocalDateTime, ZonedDateTime}
import java.time.format.DateTimeFormatter
import java.time.temporal.ChronoUnit

import cats.data.NonEmptyList
import cats.syntax.all.*
import io.circe.{Decoder, DecodingFailure, Encoder, Json}
import io.circe.Json.obj
import io.circe.syntax.*
import io.dantb.contentless.*
import io.dantb.contentless.RichText.Mark
import io.dantb.contentless.Validation.Regexp
import io.dantb.contentless.appearance.*
import io.dantb.contentless.appearance.FieldControlSetting.*
import io.dantb.contentless.codecs.EntryCodec
import io.dantb.contentless.codecs.FieldCodec.DefaultZone

object implicits:

  given zonedDateTimeEncoder: Encoder[ZonedDateTime] = zdt =>
    Json.fromString(
      zdt
        .withZoneSameInstant(DefaultZone)
        .truncatedTo(ChronoUnit.MILLIS)
        .format(DateTimeFormatter.ISO_INSTANT)
    )

  given zonedDateTimeDecoder: Decoder[ZonedDateTime]      = Decoder.decodeZonedDateTime
  def contentfulListDecoder[A: Decoder]: Decoder[List[A]] = _.get[List[A]]("items")
  given contentTypeIdEncoder: Encoder[ContentTypeId]      = Encoder[String].contramap(_.asString)
  given contentTypeIdDecoder: Decoder[ContentTypeId]      = Decoder[String].map(ContentTypeId.apply)
  given mimeTypeGroupEncoder: Encoder[MimeTypeGroup]      = Encoder[String].contramap(_.typeName)
  given mimeTypeGroupDecoder: Decoder[MimeTypeGroup] = Decoder[String].emap {
    case "archive"      => MimeTypeGroup.Archive.asRight
    case "attachment"   => MimeTypeGroup.Attachment.asRight
    case "audio"        => MimeTypeGroup.Audio.asRight
    case "code"         => MimeTypeGroup.Code.asRight
    case "image"        => MimeTypeGroup.Image.asRight
    case "markup"       => MimeTypeGroup.Markup.asRight
    case "pdfdocument"  => MimeTypeGroup.Pdfdocument.asRight
    case "plaintext"    => MimeTypeGroup.Plaintext.asRight
    case "presentation" => MimeTypeGroup.Presentation.asRight
    case "richtext"     => MimeTypeGroup.Richtext.asRight
    case "spreadsheet"  => MimeTypeGroup.Spreadsheet.asRight
    case "video"        => MimeTypeGroup.Video.asRight
    case other          => s"Unknown mime type: $other".asLeft
  }

  given markEncoder: Encoder[RichText.Mark] = mark =>
    obj(
      "type" -> mark.asString.asJson
    )

  implicit def richTextEncoder[A <: RichText.Node]: Encoder[A] = {
    case RichText.Text(value, marks) =>
      obj(
        "nodeType" -> "text".asJson,
        "value"    -> value.asJson,
        "marks"    -> marks.asJson,
        "data"     -> obj()
      )
    case RichText.Heading1(content) =>
      obj(
        "nodeType" -> RichTextNodeType.Heading1.asJson,
        "content"  -> content.asJson,
        "data"     -> obj()
      )
    case RichText.Heading2(content) =>
      obj(
        "nodeType" -> RichTextNodeType.Heading2.asJson,
        "content"  -> content.asJson,
        "data"     -> obj()
      )
    case RichText.Heading3(content) =>
      obj(
        "nodeType" -> RichTextNodeType.Heading3.asJson,
        "content"  -> content.asJson,
        "data"     -> obj()
      )
    case RichText.Heading4(content) =>
      obj(
        "nodeType" -> RichTextNodeType.Heading4.asJson,
        "content"  -> content.asJson,
        "data"     -> obj()
      )
    case RichText.Heading5(content) =>
      obj(
        "nodeType" -> RichTextNodeType.Heading5.asJson,
        "content"  -> content.asJson,
        "data"     -> obj()
      )
    case RichText.Heading6(content) =>
      obj(
        "nodeType" -> RichTextNodeType.Heading6.asJson,
        "content"  -> content.asJson,
        "data"     -> obj()
      )
    case RichText.Paragraph(content) =>
      obj(
        "nodeType" -> "paragraph".asJson,
        "content"  -> content.asJson,
        "data"     -> obj()
      )
    case RichText.Quote(content) =>
      obj(
        "nodeType" -> RichTextNodeType.Quote.asJson,
        "content"  -> content.asJson,
        "data"     -> obj()
      )
    case RichText.Hr(content) =>
      obj(
        "nodeType" -> RichTextNodeType.Hr.asJson,
        "content"  -> content.asJson,
        "data"     -> obj()
      )
    case RichText.ListItem(content) =>
      obj(
        "nodeType" -> "list-item".asJson,
        "content"  -> content.asJson,
        "data"     -> obj()
      )
    case RichText.OrderedList(content) =>
      obj(
        "nodeType" -> RichTextNodeType.OrderedList.asJson,
        "content"  -> content.asJson,
        "data"     -> obj()
      )
    case RichText.UnorderedList(content) =>
      obj(
        "nodeType" -> RichTextNodeType.UnorderedList.asJson,
        "content"  -> content.asJson,
        "data"     -> obj()
      )
    case RichText.Document(content) =>
      obj(
        "nodeType" -> "document".asJson,
        "content"  -> content.asJson,
        "data"     -> obj()
      )
    case RichText.EntryLinkBlock(content, reference) =>
      obj(
        "nodeType" -> RichTextNodeType.EntryLinkBlock.asJson,
        "content"  -> content.asJson,
        "data"     -> obj("target" -> reference.asJson)
      )
    case RichText.AssetLinkBlock(content, reference) =>
      obj(
        "nodeType" -> RichTextNodeType.AssetLinkBlock.asJson,
        "content"  -> content.asJson,
        "data"     -> obj("target" -> reference.asJson)
      )
    case RichText.EntryLinkInline(content, reference) =>
      obj(
        "nodeType" -> RichTextNodeType.EntryLinkInline.asJson,
        "content"  -> content.asJson,
        "data"     -> obj("target" -> reference.asJson)
      )
    case RichText.Hyperlink(content, uri) =>
      obj(
        "nodeType" -> RichTextNodeType.Hyperlink.asJson,
        "content"  -> content.asJson,
        "data"     -> obj("uri" -> uri.asJson)
      )
    case RichText.AssetHyperlink(content, reference) =>
      obj(
        "nodeType" -> RichTextNodeType.AssetHyperlink.asJson,
        "content"  -> content.asJson,
        "data"     -> obj("target" -> reference.asJson)
      )
    case RichText.EntryHyperlink(content, reference) =>
      obj(
        "nodeType" -> RichTextNodeType.EntryHyperlink.asJson,
        "content"  -> content.asJson,
        "data"     -> obj("target" -> reference.asJson)
      )
  }

  given markDecoder: Decoder[RichText.Mark] =
    _.downField("type")
      .as[String]
      .flatMap { str =>
        RichText.Mark
          .from(str)
          .toRight(DecodingFailure(s"Invalid Mark: $str", Nil))
      }

  given paragraphDecoder: Decoder[RichText.Paragraph] = c =>
    c.get[String]("nodeType").flatMap {
      case "paragraph" => c.get[List[RichText.Node]]("content").map(RichText.Paragraph.apply)
      case other       => Left(DecodingFailure(s"Invalid paragraph nodeType: $other", Nil))
    }

  given textDecoder: Decoder[RichText.Text] = c =>
    c.get[String]("nodeType").flatMap {
      case "text" => (c.get[String]("value"), c.get[List[RichText.Mark]]("marks")).mapN(RichText.Text.apply)
      case other  => Left(DecodingFailure(s"Invalid text nodeType: $other", Nil))
    }

  given listItemDecoder: Decoder[RichText.ListItem] = c =>
    c.get[String]("nodeType").flatMap {
      case "list-item" => c.get[List[RichText.Node]]("content").map(RichText.ListItem.apply)
      case other       => Left(DecodingFailure(s"Invalid list-item nodeType: $other", Nil))
    }

  given hrDecoder: Decoder[RichText.Hr] = c =>
    c.get[String]("nodeType").flatMap {
      case "hr"  => c.get[List[RichText.Hr]]("content").map(RichText.Hr.apply)
      case other => Left(DecodingFailure(s"Invalid hr nodeType: $other", Nil))
    }

  given richTextDecoder: Decoder[RichText.Node] = c =>
    c.downField("nodeType").as[String].flatMap {
      case "text"                             => c.as[RichText.Text]
      case RichTextNodeType.Heading1.asString => c.downField("content").as[List[RichText.Node]].map(RichText.Heading1.apply)
      case RichTextNodeType.Heading2.asString => c.downField("content").as[List[RichText.Node]].map(RichText.Heading2.apply)
      case RichTextNodeType.Heading3.asString => c.downField("content").as[List[RichText.Node]].map(RichText.Heading3.apply)
      case RichTextNodeType.Heading4.asString => c.downField("content").as[List[RichText.Node]].map(RichText.Heading4.apply)
      case RichTextNodeType.Heading5.asString => c.downField("content").as[List[RichText.Node]].map(RichText.Heading5.apply)
      case RichTextNodeType.Heading6.asString => c.downField("content").as[List[RichText.Node]].map(RichText.Heading6.apply)
      case "paragraph"                        => c.as[RichText.Paragraph]
      case RichTextNodeType.Quote.asString => c.downField("content").as[List[RichText.Paragraph]].map(RichText.Quote.apply)
      case RichTextNodeType.Hr.asString    => c.as[RichText.Hr]
      case RichTextNodeType.OrderedList.asString =>
        c.downField("content").as[List[RichText.ListItem]].map(RichText.OrderedList.apply)
      case RichTextNodeType.UnorderedList.asString =>
        c.downField("content").as[List[RichText.ListItem]].map(RichText.UnorderedList.apply)
      case "list-item" => c.as[RichText.ListItem]
      case "document"  => c.downField("content").as[List[RichText.Node]].map(RichText.Document.apply)
      case RichTextNodeType.EntryLinkBlock.asString =>
        (
          c.downField("content").as[List[RichText.Node]],
          c.downField("data").downField("target").as[Reference]
        ).mapN(RichText.EntryLinkBlock.apply)
      case RichTextNodeType.AssetLinkBlock.asString =>
        (
          c.downField("content").as[List[RichText.Node]],
          c.downField("data").downField("target").as[Reference]
        ).mapN(RichText.AssetLinkBlock.apply)
      case RichTextNodeType.EntryLinkInline.asString =>
        (
          c.downField("content").as[List[RichText.Text]],
          c.downField("data").downField("target").as[Reference]
        ).mapN(RichText.EntryLinkInline.apply)
      case RichTextNodeType.Hyperlink.asString =>
        (
          c.downField("content").as[List[RichText.Text]],
          c.downField("data").downField("uri").as[String]
        ).mapN(RichText.Hyperlink.apply)
      case RichTextNodeType.AssetHyperlink.asString =>
        (
          c.downField("content").as[List[RichText.Text]],
          c.downField("data").downField("target").as[Reference]
        ).mapN(RichText.AssetHyperlink.apply)
      case RichTextNodeType.EntryHyperlink.asString =>
        (
          c.downField("content").as[List[RichText.Text]],
          c.downField("data").downField("target").as[Reference]
        ).mapN(RichText.EntryHyperlink.apply)
    }

  implicit def fieldTypeEncoder[A <: FieldType]: Encoder[A] = {
    case t @ FieldType.Text(true, _, _, _, _) =>
      obj(
        "type"        -> "Text".asJson,
        "validations" -> t.validations.asJson
      )
    case t @ FieldType.Text(false, _, _, _, _) =>
      obj(
        "type"        -> "Symbol".asJson,
        "validations" -> t.validations.asJson
      )
    case r: FieldType.RichText =>
      obj(
        "type"        -> "RichText".asJson,
        "validations" -> r.validations.asJson
      )
    case FieldType.Media(mimeTypeGroup) =>
      obj(
        "type"        -> "Link".asJson,
        "linkType"    -> "Asset".asJson,
        "validations" -> Json.arr(obj("linkMimetypeGroup" -> mimeTypeGroup.asJson))
      )
    case i: FieldType.Integer =>
      obj(
        "type"        -> "Integer".asJson,
        "validations" -> i.validations.asJson
      )
    case n: FieldType.Number =>
      obj(
        "type"        -> "Number".asJson,
        "validations" -> n.validations.asJson
      )
    case FieldType.Boolean =>
      obj(
        "type"        -> "Boolean".asJson,
        "validations" -> Json.arr()
      )
    case j: FieldType.Json =>
      obj(
        "type"        -> "Object".asJson,
        "validations" -> j.validations.asJson
      )
    case d: FieldType.DateTime =>
      obj(
        "type"        -> "Date".asJson,
        "validations" -> d.validations.asJson
      )
    case FieldType.Location => obj("type" -> "Location".asJson)
    case FieldType.Reference(linkContentTypes) =>
      obj(
        "type"        -> "Link".asJson,
        "linkType"    -> "Entry".asJson,
        "validations" -> Json.arr(obj("linkContentType" -> linkContentTypes.asJson))
      )
    case a @ FieldType.Array(itemType, _) =>
      obj(
        "type"        -> "Array".asJson,
        "items"       -> itemType.asJson(fieldTypeEncoder),
        "validations" -> a.validations.asJson
      )
  }

  final case class Validations(s: Set[Validation])
  given Decoder[Validations] = c =>
    c.downField("validations").success.map(_.as[Set[Validation]].map(Validations(_))).getOrElse(Right(Validations(Set())))

  given fieldTypeDecoder: Decoder[FieldType] = c =>
    c.downField("type").as[String].flatMap {
      case "Text"     => c.as[Validations].map(v => FieldType.Text.fromValidations(true, v.s))
      case "Symbol"   => c.as[Validations].map(v => FieldType.Text.fromValidations(false, v.s))
      case "Integer"  => c.as[Validations].map(v => FieldType.Integer.fromValidations(v.s))
      case "Number"   => c.as[Validations].map(v => FieldType.Number.fromValidations(v.s))
      case "Boolean"  => FieldType.Boolean.asRight
      case "Object"   => c.as[Validations].map(v => FieldType.Json.fromValidations(v.s))
      case "Date"     => c.as[Validations].map(v => FieldType.DateTime.fromValidations(v.s))
      case "Location" => FieldType.Location.asRight
      case "RichText" => c.as[Validations].map(v => FieldType.RichText.fromValidations(v.s))
      case "Link" =>
        c.downField("linkType").as[String].flatMap {
          case "Asset" =>
            c
              .downField("validations")
              .downArray
              .downField("linkMimetypeGroup")
              .as[Set[MimeTypeGroup]]
              .map[FieldType](FieldType.Media.apply)
          case "Entry" =>
            c
              .downField("validations")
              .downArray
              .downField("linkContentType")
              .as[Set[ContentTypeId]]
              .map[FieldType](FieldType.Reference.apply)
          case other => DecodingFailure(s"Unknown link type: $other", Nil).asLeft
        }

      case "Array" =>
        val size = c.downField("validations").downArray.as[Validation.Size]
        c.downField("items").as[FieldType](fieldTypeDecoder).map(items => FieldType.Array(items, size.toOption))
    }

  given fieldEncoder: Encoder[Field] = field =>
    val typeFields = field.fieldType.asJson

    obj(
      "id"        -> field.id.asJson,
      "name"      -> field.name.asJson,
      "localized" -> false.asJson,
      "required"  -> field.required.asJson,
      "disabled"  -> field.disabled.asJson
    )
      .deepMerge(typeFields)
      .mapObject(if field.defaultValue.isEmpty then identity else _.add("defaultValue", field.defaultValue.asJson))

  given fieldDecoder: Decoder[Field] = c =>
    (
      c.get[String]("id"),
      c.get[String]("name"),
      c.get[Boolean]("required"),
      c.get[Boolean]("disabled"),
      c.as[FieldType],
      c.get[Option[Map[String, Json]]]("defaultValue").map(_.getOrElse(Map.empty))
    ).mapN(Field.apply)

  given referenceEncoder: Encoder[Reference] = ref =>
    obj("sys" -> obj("type" -> "Link".asJson, "linkType" -> "Entry".asJson, "id" -> ref.id.asJson))

  given referenceDecoder: Decoder[Reference] = c => c.downField("sys").downField("id").as[String].map(Reference.apply)

  given locationEncoder: Encoder[Location] = location =>
    obj("lat" -> location.latitude.asJson, "lon" -> location.longitude.asJson)

  given locationDecoder: Decoder[Location] = c =>
    (c.downField("lat").as[Double], c.downField("lon").as[Double]).mapN(Location.apply)

  given mediaEncoder: Encoder[Media] = media =>
    obj("sys" -> obj("type" -> "Link".asJson, "linkType" -> "Asset".asJson, "id" -> media.id.asJson))

  given mediaDecoder: Decoder[Media] = _.downField("sys").downField("id").as[String].map(Media.apply)

  given validationSizeDecoder: Decoder[Validation.Size] = c =>
    val size = c.downField("size")
    if size.succeeded then
      for
        min     <- size.downField("min").as[Option[Int]]
        max     <- size.downField("max").as[Option[Int]]
        message <- c.downField("message").as[Option[String]]
      yield Validation.Size(min, max, message, "size")
    else
      val range = c.downField("range")
      for
        min     <- range.downField("min").as[Option[Int]]
        max     <- range.downField("max").as[Option[Int]]
        message <- c.downField("message").as[Option[String]]
      yield Validation.Size(min, max, message, "range")

  given regexpDecoder: Decoder[Regexp] = Decoder[String].map(Regexp.of)

  given validationRegexpDecoder: Decoder[Validation.RegexpValidation] = c =>
    for
      pattern <- c.downField("regexp").get[Regexp]("pattern")
      message <- c.get[Option[String]]("message")
    yield Validation.RegexpValidation(pattern, message)

  given Decoder[Validation.DateRange] = c =>
    final case class AnyDateTime(zdt: ZonedDateTime)
    given Decoder[AnyDateTime] = Decoder[ZonedDateTime]
      .or(Decoder[LocalDateTime].map(_.atZone(DefaultZone)))
      .or(Decoder[LocalDate].map(_.atStartOfDay(DefaultZone)))
      .map(AnyDateTime(_))
    for
      min <- c.downField("dateRange").get[Option[AnyDateTime]]("min")
      max <- c.downField("dateRange").get[Option[AnyDateTime]]("max")
    yield Validation.DateRange(min.map(_.zdt), max.map(_.zdt))

  given Decoder[RichTextNodeType] =
    Decoder[String].emap(r => RichTextNodeType.from(r).toRight(s"Invalid rich text node type: $r"))

  given Encoder[RichTextNodeType] = Encoder[String].contramap(_.asString)

  given richTextNodesValidationDecoder: Decoder[Validation.RichTextNodes] = c =>
    import Validation.*
    def parseNodeEntryValidation[A](
        key: String,
        constructA: (Option[Size], Option[LinkContentType]) => A
    ): Either[DecodingFailure, Option[A]] =
      c.downField("nodes").downField(key).as[Option[List[Validation]]].map { maybeValidations =>
        maybeValidations.map(validations =>
          constructA(
            validations.collectFirst { case size: Size => size },
            validations.collectFirst { case lct: LinkContentType => lct }
          )
        )
      }

    for
      assetHyperlinkSize <- c
        .downField("nodes")
        .downField("asset-hyperlink")
        .as[Option[List[Size]]]
        .map(_.flatMap(_.headOption))
      assetBlockSize <- c
        .downField("nodes")
        .downField("embedded-asset-block")
        .as[Option[List[Size]]]
        .map(_.flatMap(_.headOption))
      entryHyperlink <- parseNodeEntryValidation("entry-hyperlink", RichTextNodes.EntryHyperlink.apply)
      entryInline    <- parseNodeEntryValidation("embedded-entry-inline", RichTextNodes.EntryInline.apply)
      entryBlock     <- parseNodeEntryValidation("embedded-entry-block", RichTextNodes.EntryBlock.apply)
    yield Validation.RichTextNodes(assetHyperlinkSize, entryHyperlink, assetBlockSize, entryBlock, entryInline)

  given validationDecoder: Decoder[Validation] = c =>
    c.downField("in")
      .success
      .map(x =>
        x.as[NonEmptyList[String]]
          .map[Validation](Validation.ContainedIn.apply)
          .orElse(x.as[NonEmptyList[Int]].map[Validation](Validation.ContainedInInt.apply))
          .orElse(x.as[NonEmptyList[Double]].map[Validation](Validation.ContainedInDecimal.apply))
      ) orElse
      c.downField("enabledMarks")
        .success
        .map(
          _.as[Set[String]]
            .flatMap(_.toList.traverse(Mark.from(_).toRight(DecodingFailure("Invalid mark", Nil))))
            .map[Validation](x => Validation.RichTextMarks(x.toSet))
        ) orElse
      c
        .downField("enabledNodeTypes")
        .success
        .map(_.as[Set[RichTextNodeType]].map[Validation](Validation.RichTextNodeTypes.apply)) orElse
      c.downField("unique").success.map(_.as[Boolean].as[Validation](Validation.Unique)) orElse
      c.downField("regexp").success.as(c.as[Validation.RegexpValidation]) orElse
      c.downField("size").success.as(c.as[Validation.Size]) orElse
      c.downField("range").success.as(c.as[Validation.Size]) orElse
      c.downField("dateRange").success.as(c.as[Validation.DateRange]) orElse
      c.downField("nodes").success.as(c.as[Validation.RichTextNodes]) orElse
      c.downField("linkContentType").success.as(c.as[Validation.LinkContentType]) getOrElse
      DecodingFailure(s"Failed to match a validation rule on $c", Nil).asLeft

  given linkContentTypeEncoder: Encoder[Validation.LinkContentType] = lct =>
    obj(
      "linkContentType" -> lct.allowedContentTypes.asJson,
      "message"         -> lct.message.asJson
    )

  given linkContentTypeDecoder: Decoder[Validation.LinkContentType] = c =>
    for
      types   <- c.downField("linkContentType").as[Set[String]]
      message <- c.downField("message").as[Option[String]]
    yield Validation.LinkContentType(types, message)

  implicit def validationEncoder[A <: Validation]: Encoder[A] = {
    case Validation.ContainedIn(allowedValues) =>
      obj("in" -> allowedValues.asJson)
    case Validation.ContainedInInt(allowedValues) =>
      obj("in" -> allowedValues.asJson)
    case Validation.ContainedInDecimal(allowedValues) =>
      obj("in" -> allowedValues.asJson)
    case v @ Validation.RichTextMarks(enabledMarks) =>
      obj(
        "enabledMarks" -> enabledMarks.map(_.asString).asJson,
        "message"      -> v.message.asJson
      )
    case v @ Validation.RichTextNodeTypes(enabledNodeTypes) =>
      obj(
        "enabledNodeTypes" -> enabledNodeTypes.asJson,
        "message"          -> v.message.asJson
      )
    case Validation.Unique =>
      obj("unique" -> true.asJson)
    case Validation.RegexpValidation(regexp, message) =>
      obj(
        "regexp" -> obj(
          "pattern" -> regexp.underlying.toString().asJson
        ),
        "message" -> message.asJson
      )
    case Validation.Size(min, max, message, tpe) =>
      obj(
        tpe -> obj(
          "min" -> min.asJson,
          "max" -> max.asJson
        ),
        "message" -> message.asJson
      )
    case Validation.DateRange(min, max) =>
      obj(
        "dateRange" -> obj(
          "min" -> min.asJson,
          "max" -> max.asJson
        )
      )
    case Validation.LinkContentType(allowedContentTypes, message) =>
      obj(
        "linkContentType" -> allowedContentTypes.asJson,
        "message"         -> message.asJson
      )
    case Validation.RichTextNodes(assetHyperlinkSize, entryHyperlink, assetBlock, entryBlock, entryInline) =>
      import Validation.*
      import Validation.RichTextNodes.*

      def encodeNodeEntryValidation[B](
          entry: Option[B],
          key: String,
          extractSize: B => Option[Size],
          extractLinkContentType: B => Option[LinkContentType]
      ): Json =
        entry
          .map { elem =>
            obj(
              key -> List(
                extractLinkContentType(elem).map(_.asJson),
                extractSize(elem).map(_.asJson(validationEncoder))
              ).flatten.asJson
            )
          }
          .getOrElse(obj())

      val assetHyperlinkJson: Json =
        assetHyperlinkSize.map(s => obj("asset-hyperlink" -> List(s).asJson)).getOrElse(obj())
      val assetBlockJson: Json =
        assetBlock.map(s => obj("embedded-asset-block" -> List(s).asJson)).getOrElse(obj())
      val entryHyperlinkJson =
        encodeNodeEntryValidation[EntryHyperlink](
          entry = entryHyperlink,
          key = "entry-hyperlink",
          extractSize = _.size,
          extractLinkContentType = _.linkContentType
        )
      val entryBlockJson =
        encodeNodeEntryValidation[EntryBlock](
          entry = entryBlock,
          key = "embedded-entry-block",
          extractSize = _.size,
          extractLinkContentType = _.linkContentType
        )
      val entryInlineJson =
        encodeNodeEntryValidation[EntryInline](
          entry = entryInline,
          key = "embedded-entry-inline",
          extractSize = _.size,
          extractLinkContentType = _.linkContentType
        )

      val nodes = assetHyperlinkJson
        .deepMerge(assetBlockJson)
        .deepMerge(entryHyperlinkJson)
        .deepMerge(entryBlockJson)
        .deepMerge(entryInlineJson)
      obj(
        "nodes" -> nodes
      )
  }
