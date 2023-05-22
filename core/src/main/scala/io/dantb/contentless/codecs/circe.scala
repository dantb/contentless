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
import io.dantb.contentless.Entry.{Authors, Timestamps}
import io.dantb.contentless.RichText.Mark
import io.dantb.contentless.Validation.Regexp
import io.dantb.contentless.appearance.*
import io.dantb.contentless.appearance.FieldControlSetting.*
import io.dantb.contentless.codecs.EntryCodec
import io.dantb.contentless.codecs.FieldCodec.DefaultZone

object implicits:
  implicit def contentfulEntryEncoder[A](using codec: EntryCodec[A]): Encoder[Entry[A]] = value =>
    obj(
      "fields" -> codec.write(value.fields).asJson
    )

  given timestampsDecoder: Decoder[Timestamps] = c =>
    for
      createdAt        <- c.downField("createdAt").as[Option[ZonedDateTime]]
      updatedAt        <- c.downField("updatedAt").as[Option[ZonedDateTime]]
      publishedAt      <- c.downField("publishedAt").as[Option[ZonedDateTime]]
      firstPublishedAt <- c.downField("firstPublishedAt").as[Option[ZonedDateTime]]
    yield Timestamps(
      createdAt = createdAt,
      updatedAt = updatedAt,
      publishedAt = publishedAt,
      firstPublishedAt = firstPublishedAt
    )
  given authorsDecoder: Decoder[Authors] = c =>
    for
      createdBy   <- c.downField("createdBy").as[Option[Reference]]
      updatedBy   <- c.downField("updatedBy").as[Option[Reference]]
      publishedBy <- c.downField("publishedBy").as[Option[Reference]]
    yield Authors(
      createdBy = createdBy,
      updatedBy = updatedBy,
      publishedBy = publishedBy
    )

  implicit def contentfulEntryDecoder[A](using codec: EntryCodec[A]): Decoder[Entry[A]] = c =>
    for
      id <- c.downField("sys").get[Option[String]]("id")
      fields <- c
        .get[Map[String, Json]]("fields")
        .flatMap(json => codec.read(json).leftMap(err => DecodingFailure(err, Nil)))
      version    <- c.downField("sys").get[Option[Int]]("version")
      space      <- c.downField("sys").get[Option[Reference]]("space")
      env        <- c.downField("sys").get[Option[Reference]]("environment")
      timestamps <- c.get[Timestamps]("sys")
      authors    <- c.get[Authors]("sys")
    yield Entry[A](
      id = id,
      fields = fields,
      version = version,
      space = space,
      environment = env,
      timestamps = timestamps,
      authors = authors
    )

  implicit def contentfulEntriesAndIncludesDecoder[A](using
      decoder: Decoder[Entry[A]]
  ): Decoder[EntriesAndIncludes[A]] = c =>
    (
      c.get[List[Entry[A]]]("items"),
      c.get[Int]("skip"),
      c.get[Int]("limit"),
      c.get[Int]("total")
    ).mapN { case (entries, skip, limit, total) =>
      EntriesAndIncludes[A](
        entries = entries,
        includedEntries = c.downField("includes").get[List[Json]]("Entry").getOrElse(Nil),
        includedAssets = c.downField("includes").get[List[Json]]("Asset").getOrElse(Nil),
        skip = skip,
        limit = limit,
        total = total
      )
    }

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

  given assetDataEncoder: Encoder[File] = data =>
    obj(
      "contentType" -> data.contentType.asJson,
      "fileName"    -> data.fileName.asJson,
      "upload"      -> data.upload.asJson
    )

  given assetDataDecoder: Decoder[File] = c =>
    (
      c.downField("contentType").as[String],
      c.downField("fileName").as[String],
      c.get[String]("upload").orElse(c.get[String]("url"))
    ).mapN(File.apply)

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

  given contentTypeEncoder: Encoder[ContentType] = ct =>
    obj(
      "displayField" -> ct.displayField.asJson,
      "name"         -> ct.name.asJson,
      "description"  -> ct.description.asJson,
      "fields"       -> ct.fields.asJson
    )

  given contentTypeDecoder: Decoder[ContentType] = c =>
    for
      id           <- c.downField("sys").downField("id").as[ContentTypeId]
      name         <- c.downField("name").as[String]
      displayField <- c.downField("displayField").as[Option[String]]
      description  <- c.downField("description").as[Option[String]]
      fields       <- c.downField("fields").as[List[Field]]
      version      <- c.downField("sys").downField("version").as[Option[Int]]
    yield ContentType(
      id = id,
      name = name,
      displayField = displayField,
      description = description,
      fields = fields,
      version = version
    )

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

  given controlEnc: Encoder[FieldControl] = fc =>
    val maybeSettings: Option[Json] =
      if fc.settings.nonEmpty then
        Some(fc.settings.map(setting => setting.name -> encodeSettingValue(setting)).toMap.asJson)
      else None

    Json.fromFields(
      List(
        "fieldId"         -> fc.fieldId.asJson,
        "widgetId"        -> fc.control.id.asJson,
        "widgetNamespace" -> fc.control.namespace.asJson
      ) ++ maybeSettings.map("settings" -> _)
    )

  def encodeSettingValue(setting: FieldControlSetting): Json =
    setting match
      case Rating.Stars(value)                       => value.asJson
      case DatePicker.Format(value)                  => value.asJson
      case LinksEditor.BulkEditing(value)            => value.asJson
      case FieldControlSetting.HelpText(value)       => value.asJson
      case DatePicker.ClockType(value)               => value.asJson
      case Boolean.TrueLabel(value)                  => value.asJson
      case Boolean.FalseLabel(value)                 => value.asJson
      case LinksEditor.ShowLinkEntityAction(value)   => value.asJson
      case LinksEditor.ShowCreateEntityAction(value) => value.asJson
      case setting: CustomSetting                    => setting.toJson

  given sidebarEnc: Encoder[SidebarWidget] = control =>
    obj("widgetId" -> control.id.asJson, "widgetNamespace" -> control.namespace.asJson)

  given editorEnc: Encoder[Editor] = control =>
    val disabled = if control.disabled then Some("disabled" -> true.asJson) else None
    Json.fromFields(
      List(
        "widgetId"        -> control.id.asJson,
        "widgetNamespace" -> control.namespace.asJson
      ) ++ disabled // don't include if it's not disabled (from the API's current behaviour)
    )

  given interfaceEnc: Encoder[EditorInterface] = interface =>
    def encNonEmpty[A: Encoder](key: String, list: List[A]): Option[(String, Json)] =
      if list.nonEmpty then Some(key -> list.asJson) else None

    Json.fromFields(
      encNonEmpty("editors", interface.editors.toList) ++
        encNonEmpty("sidebar", interface.sidebarWidgets) ++
        encNonEmpty("controls", interface.controls.toList)
    )

  given datePickerClockTypeDec: Decoder[DatePicker.ClockType] = Decoder[String].emap(s =>
    DatePicker.ClockType.parse(s).toRight(s"Invalid ${DatePicker.ClockType.Name} field setting: $s")
  )

  given datePickerFormatDec: Decoder[DatePicker.Format] =
    Decoder[String].emap(s => DatePicker.Format.parse(s).toRight(s"Invalid ${DatePicker.Format.Name} field setting: $s"))

  given controlDec: Decoder[FieldControl] = c =>
    for
      fieldId   <- c.get[String]("fieldId")
      widgetId  <- c.get[String]("widgetId")
      namespace <- c.get[String]("widgetNamespace")
      control <- Control
        .parse(namespace, widgetId)
        .toRight(DecodingFailure(s"Invalid widget $widgetId in namespace $namespace for field $fieldId", Nil))
      maybeSettings <- c
        .get[Option[Map[String, Json]]]("settings")
        .flatMap(_.traverse(_.toList.traverse { case (k, json) => parseFieldControlSetting(k, json) }))
        .map(_.map(_.toSet))
    yield FieldControl(fieldId, control, maybeSettings.getOrElse(Set.empty))

  def parseFieldControlSetting(key: String, json: Json): Decoder.Result[FieldControlSetting] =
    import FieldControlSetting.*
    key match
      case Boolean.TrueLabel.Name                  => json.as[String].map(Boolean.TrueLabel.apply)
      case Boolean.FalseLabel.Name                 => json.as[String].map(Boolean.FalseLabel.apply)
      case Rating.Stars.Name                       => json.as[Int].map(Rating.Stars.apply)
      case DatePicker.Format.Name                  => json.as[DatePicker.Format]
      case DatePicker.ClockType.Name               => json.as[DatePicker.ClockType]
      case HelpText.Name                           => json.as[String].map(HelpText.apply)
      case LinksEditor.BulkEditing.Name            => json.as[Boolean].map(LinksEditor.BulkEditing.apply)
      case LinksEditor.ShowLinkEntityAction.Name   => json.as[Boolean].map(LinksEditor.ShowLinkEntityAction.apply)
      case LinksEditor.ShowCreateEntityAction.Name => json.as[Boolean].map(LinksEditor.ShowCreateEntityAction.apply)
      case name => Right(CustomSetting(name, json)) // assume any other setting is a custom setting.

  given sidebarDec: Decoder[SidebarWidget] = c =>
    for
      widgetId  <- c.get[String]("widgetId")
      namespace <- c.get[String]("widgetNamespace")
      sidebar <- SidebarWidget
        .parse(namespace, widgetId)
        .toRight(DecodingFailure(s"Invalid sidebar widget $widgetId for namespace $namespace", Nil))
    yield sidebar

  given editorDec: Decoder[Editor] = c =>
    for
      widgetId  <- c.get[String]("widgetId")
      namespace <- c.get[String]("widgetNamespace")
      disabled  <- c.get[Option[Boolean]]("disabled")
      editor <- Editor
        .parse(namespace, widgetId, disabled.getOrElse(false))
        .toRight(DecodingFailure(s"Invalid editor widget $widgetId for namespace $namespace", Nil))
    yield editor

  given interfaceDec: Decoder[EditorInterface] = c =>
    val editorsCur  = c.downField("editors")
    val sidebarCur  = c.downField("sidebar")
    val controlsCur = c.downField("controls")
    for
      editors  <- if editorsCur.succeeded then editorsCur.as[Set[Editor]] else Right(Set.empty[Editor])
      sidebars <- if sidebarCur.succeeded then sidebarCur.as[List[SidebarWidget]] else Right(Nil)
      controls <-
        if controlsCur.succeeded then
          controlsCur.as[List[Json]].flatMap(_.flatTraverse(swallowFieldsWithoutWidget)).map(_.toSet)
        else Right(Set.empty[FieldControl])
    yield EditorInterface(editors, sidebars, controls)

  /** Bit of a hack working around Contentful bug. Weird behaviour where only the fieldId is returned, no widget. This means
    * it is the default widget for the field type. If declared from code it will be explicitly set.
    */
  def swallowFieldsWithoutWidget(json: Json): Decoder.Result[List[FieldControl]] =
    json
      .as[FieldControl]
      .map(List(_))
      .recoverWith[DecodingFailure, List[FieldControl]] { case err =>
        json.hcursor
          .get[String]("fieldId")
          .fold(
            fa = _ => Left(err),
            fb = _ => Right(Nil)
          )
      }

  given versionedInterfaceEnc: Encoder[VersionedEditorInterface] =
    Encoder[EditorInterface].contramap(_.editorInterface)

  given versionedInterfaceDec: Decoder[VersionedEditorInterface] = c =>
    (
      c.downField("sys").downField("version").as[Int],
      c.downField("sys").downField("contentType").downField("sys").downField("id").as[ContentTypeId],
      c.as[EditorInterface]
    ).mapN(VersionedEditorInterface.apply)
