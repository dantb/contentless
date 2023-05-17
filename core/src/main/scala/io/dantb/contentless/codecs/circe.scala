package io.dantb.contentless.codecs

import java.time.{ZoneId, ZonedDateTime}
import java.time.format.DateTimeFormatter
import java.time.temporal.ChronoUnit

import cats.data.NonEmptyList
import cats.syntax.all.*
import io.circe.{Decoder, DecodingFailure, Encoder, Json}
import io.circe.Json.obj
import io.circe.syntax.*
import io.dantb.contentless.*
import io.dantb.contentless.Entry.{Authors, Timestamps}
import io.dantb.contentless.appearance.*
import io.dantb.contentless.appearance.FieldControlSetting.*
import io.dantb.contentless.codecs.EntryCodec
import io.dantb.contentless.webhook.*

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
        .withZoneSameInstant(ZoneId.of("UTC"))
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
      "type" -> mark.`type`.asJson
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
        "nodeType" -> "heading-1".asJson,
        "content"  -> content.asJson,
        "data"     -> obj()
      )
    case RichText.Heading2(content) =>
      obj(
        "nodeType" -> "heading-2".asJson,
        "content"  -> content.asJson,
        "data"     -> obj()
      )
    case RichText.Heading3(content) =>
      obj(
        "nodeType" -> "heading-3".asJson,
        "content"  -> content.asJson,
        "data"     -> obj()
      )
    case RichText.Heading4(content) =>
      obj(
        "nodeType" ->
          "heading-4".asJson,
        "content" -> content.asJson,
        "data"    -> obj()
      )
    case RichText.Heading5(content) =>
      obj(
        "nodeType" -> "heading-5".asJson,
        "content"  -> content.asJson,
        "data"     -> obj()
      )
    case RichText.Heading6(content) =>
      obj(
        "nodeType" -> "heading-6".asJson,
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
        "nodeType" -> "blockquote".asJson,
        "content"  -> content.asJson,
        "data"     -> obj()
      )
    case RichText.Hr(content) =>
      obj(
        "nodeType" -> "hr".asJson,
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
        "nodeType" -> "ordered-list".asJson,
        "content"  -> content.asJson,
        "data"     -> obj()
      )
    case RichText.UnorderedList(content) =>
      obj(
        "nodeType" -> "unordered-list".asJson,
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
        "nodeType" -> "embedded-entry-block".asJson,
        "content"  -> content.asJson,
        "data"     -> obj("target" -> reference.asJson)
      )
    case RichText.AssetLinkBlock(content, reference) =>
      obj(
        "nodeType" -> "embedded-asset-block".asJson,
        "content"  -> content.asJson,
        "data"     -> obj("target" -> reference.asJson)
      )
    case RichText.EntryLinkInline(content, reference) =>
      obj(
        "nodeType" -> "embedded-entry-inline".asJson,
        "content"  -> content.asJson,
        "data"     -> obj("target" -> reference.asJson)
      )
    case RichText.Hyperlink(content, uri) =>
      obj(
        "nodeType" -> "hyperlink".asJson,
        "content"  -> content.asJson,
        "data"     -> obj("uri" -> uri.asJson)
      )
    case RichText.AssetHyperlink(content, reference) =>
      obj(
        "nodeType" -> "asset-hyperlink".asJson,
        "content"  -> content.asJson,
        "data"     -> obj("target" -> reference.asJson)
      )
    case RichText.EntryHyperlink(content, reference) =>
      obj(
        "nodeType" -> "entry-hyperlink".asJson,
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
      case "text"           => c.as[RichText.Text]
      case "heading-1"      => c.downField("content").as[List[RichText.Node]].map(RichText.Heading1.apply)
      case "heading-2"      => c.downField("content").as[List[RichText.Node]].map(RichText.Heading2.apply)
      case "heading-3"      => c.downField("content").as[List[RichText.Node]].map(RichText.Heading3.apply)
      case "heading-4"      => c.downField("content").as[List[RichText.Node]].map(RichText.Heading4.apply)
      case "heading-5"      => c.downField("content").as[List[RichText.Node]].map(RichText.Heading5.apply)
      case "heading-6"      => c.downField("content").as[List[RichText.Node]].map(RichText.Heading6.apply)
      case "paragraph"      => c.as[RichText.Paragraph]
      case "blockquote"     => c.downField("content").as[List[RichText.Paragraph]].map(RichText.Quote.apply)
      case "hr"             => c.as[RichText.Hr]
      case "ordered-list"   => c.downField("content").as[List[RichText.ListItem]].map(RichText.OrderedList.apply)
      case "unordered-list" => c.downField("content").as[List[RichText.ListItem]].map(RichText.UnorderedList.apply)
      case "list-item"      => c.as[RichText.ListItem]
      case "document"       => c.downField("content").as[List[RichText.Node]].map(RichText.Document.apply)
      case "embedded-entry-block" =>
        (
          c.downField("content").as[List[RichText.Node]],
          c.downField("data").downField("target").as[Reference]
        ).mapN(RichText.EntryLinkBlock.apply)
      case "embedded-asset-block" =>
        (
          c.downField("content").as[List[RichText.Node]],
          c.downField("data").downField("target").as[Reference]
        ).mapN(RichText.AssetLinkBlock.apply)
      case "embedded-entry-inline" =>
        (
          c.downField("content").as[List[RichText.Text]],
          c.downField("data").downField("target").as[Reference]
        ).mapN(RichText.EntryLinkInline.apply)
      case "hyperlink" =>
        (
          c.downField("content").as[List[RichText.Text]],
          c.downField("data").downField("uri").as[String]
        ).mapN(RichText.Hyperlink.apply)
      case "asset-hyperlink" =>
        (
          c.downField("content").as[List[RichText.Text]],
          c.downField("data").downField("target").as[Reference]
        ).mapN(RichText.AssetHyperlink.apply)
      case "entry-hyperlink" =>
        (
          c.downField("content").as[List[RichText.Text]],
          c.downField("data").downField("target").as[Reference]
        ).mapN(RichText.EntryHyperlink.apply)
    }

  given contentTypeHeaderEncoder: Encoder[ContentTypeHeader] = Encoder[String].contramap(_.key)

  given contentTypeHeaderDecoder: Decoder[ContentTypeHeader] =
    Decoder[String].emap(s => ContentTypeHeader.parse(s).toRight(s"Invalid webhook content type $s"))

  given methodEncoder: Encoder[Method] = Encoder[String].contramap(_.asString)

  given methodDecoder: Decoder[Method] =
    Decoder[String].emap(s => Method.parse(s).toRight(s"Invalid webhook method $s"))

  given transformationEncoder: Encoder[Transformation] = t =>
    def encodeOption[A: Encoder](key: String, opt: Option[A]): Iterable[(String, Json)] =
      opt.map(v => key -> v.asJson).toIterable

    Json.fromFields(
      encodeOption("contentType", t.contentType) ++
        encodeOption("body", t.body) ++
        encodeOption("method", t.method) ++
        encodeOption("includeContentLength", t.includeContentLength)
    )

  given transformationDecoder: Decoder[Transformation] = c =>
    (
      c.get[Option[Method]]("method"),
      c.get[Option[ContentTypeHeader]]("contentType"),
      c.get[Option[Boolean]]("includeContentLength"),
      c.get[Option[Json]]("body")
    ).mapN(Transformation.apply)

  given webhookDefinitionEncoder: Encoder[WebhookDefinition] = req =>
    obj(
      "name"           -> req.name.asJson,
      "url"            -> req.url.asJson,
      "topics"         -> req.topics.asJson,
      "filters"        -> req.filters.asJson,
      "transformation" -> req.transformation.asJson,
      "headers"        -> req.headers.asJson
    )

  given webhookDefinitionDecoder: Decoder[WebhookDefinition] = c =>
    (
      c.downField("sys").get[String]("id"),
      c.get[String]("name"),
      c.get[String]("url"),
      c.get[List[WebhookTopic]]("topics"),
      c.get[List[WebhookFilter]]("filters"),
      c.get[List[WebhookHeader]]("headers"),
      c.downField("sys").get[Option[Int]]("version"),
      c.get[Option[Transformation]]("transformation").map(_.getOrElse(Transformation.empty))
    ).mapN(WebhookDefinition.apply)

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
    case FieldType.RichText(validations) =>
      obj(
        "type"        -> "RichText".asJson,
        "validations" -> validations.asJson
      )
    case FieldType.Media(mimeTypeGroup) =>
      obj(
        "type"        -> "Link".asJson,
        "linkType"    -> "Asset".asJson,
        "validations" -> Json.arr(obj("linkMimetypeGroup" -> mimeTypeGroup.asJson))
      )
    case FieldType.Integer => obj("type" -> "Integer".asJson)
    case FieldType.Number  => obj("type" -> "Number".asJson)
    case FieldType.Boolean => obj("type" -> "Boolean".asJson)
    case j: FieldType.Json =>
      obj(
        "type"        -> "Object".asJson,
        "validations" -> j.validations.asJson
      )
    case FieldType.DateTime => obj("type" -> "Date".asJson)
    case FieldType.Location => obj("type" -> "Location".asJson)
    case FieldType.Reference(linkContentTypes) =>
      obj(
        "type"        -> "Link".asJson,
        "linkType"    -> "Entry".asJson,
        "validations" -> Json.arr(obj("linkContentType" -> linkContentTypes.asJson))
      )
    case FieldType.Array(itemType, minLength, maxLength) =>
      val validations =
        (minLength, maxLength) match
          case (None, None) => List.empty[Validation]
          case _            => List[Validation](Validation.Size(minLength, maxLength, None))
      obj(
        "type"        -> "Array".asJson,
        "items"       -> itemType.asJson(fieldTypeEncoder),
        "validations" -> validations.asJson
      )
  }

  given fieldTypeDecoder: Decoder[FieldType] = c =>
    c.downField("type").as[String].flatMap {
      case "Text"     => c.downField("validations").as[Set[Validation]].map(FieldType.Text.fromValidations(true, _))
      case "Symbol"   => c.downField("validations").as[Set[Validation]].map(FieldType.Text.fromValidations(false, _))
      case "Integer"  => FieldType.Integer.asRight
      case "Number"   => FieldType.Number.asRight
      case "Boolean"  => FieldType.Boolean.asRight
      case "Object"   => c.downField("validations").as[Set[Validation]].map(FieldType.Json.fromValidations(_))
      case "Date"     => FieldType.DateTime.asRight
      case "Location" => FieldType.Location.asRight
      case "RichText" => c.downField("validations").as[Set[Validation]].map[FieldType](FieldType.RichText.apply)
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
        (
          c.downField("items").as[FieldType](fieldTypeDecoder),
          c
            .downField("validations")
            .downArray
            .downField("size")
            .get[Option[Int]]("min"),
          c
            .downField("validations")
            .downArray
            .downField("size")
            .get[Option[Int]]("max")
        ).mapN(FieldType.Array.apply)
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

  given webhookHeaderEncoder: Encoder[WebhookHeader] = header =>
    obj(
      "key"    -> header.key.asJson,
      "value"  -> header.value.asJson,
      "secret" -> header.secret.asJson
    )

  given webhookHeaderDecoder: Decoder[WebhookHeader] = c =>
    (
      c.get[String]("key"),
      c.get[Option[String]]("value"),
      c.get[Option[Boolean]]("secret").map(_.getOrElse(false))
    ).mapN(WebhookHeader.apply)

  given webhookTopicEncoder: Encoder[WebhookTopic] =
    Encoder[String].contramap(topic => s"${topic.entityType.typeName}.${topic.event.eventName}")

  given webhookTopicDecoder: Decoder[WebhookTopic] =
    Decoder[String].emap { str =>
      str.split('.') match
        case Array(typeName, eventName) =>
          (
            WebhookEvent.fromString(eventName).toRight(s"Unknown event type: $eventName"),
            EntityType.fromString(typeName).toRight(s"Unknown entity type: $typeName")
          ).mapN(WebhookTopic.apply)
        case _ => s"Invalid webhook topic: $str".asLeft
    }

  given propertyPathEncoder: Encoder[PropertyPath] = Encoder[String].contramap(_.asString)
  given propertyPathDecoder: Decoder[PropertyPath] = Decoder[String].emap { str =>
    PropertyPath.fromString(str).toRight(s"Unrecognized property path: $str")
  }

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

  given webhookFilterEncoder: Encoder[WebhookFilter] = {
    case WebhookFilter.Equals(property, operand) =>
      obj(
        "equals" -> Json.arr(
          obj("doc" -> property.asJson),
          operand.asJson
        )
      )
    case WebhookFilter.Includes(property, operands) =>
      obj(
        "in" -> Json.arr(
          obj("doc" -> property.asJson),
          operands.asJson
        )
      )
    case WebhookFilter.Regex(property, pattern) =>
      obj(
        "regexp" -> Json.arr(
          obj("doc"     -> property.asJson),
          obj("pattern" -> pattern.asJson)
        )
      )
    case WebhookFilter.Not(inner) =>
      obj(
        "not" -> inner.asJson(webhookFilterEncoder)
      )
  }

  given webhookFilterDecoder: Decoder[WebhookFilter] = c =>
    val equals = c.downField("equals").success.map { eqCur =>
      (
        eqCur.downN(0).downField("doc").as[PropertyPath],
        eqCur.downN(1).as[String]
      ).mapN(WebhookFilter.Equals.apply)
    }
    val in = c.downField("in").success.map { inCur =>
      (
        inCur.downN(0).downField("doc").as[PropertyPath],
        inCur.downN(1).as[List[String]]
      ).mapN(WebhookFilter.Includes.apply)
    }
    val regex = c.downField("regexp").success.map { regexCur =>
      (
        regexCur.downN(0).downField("doc").as[PropertyPath],
        regexCur.downN(1).downField("pattern").as[String]
      ).mapN(WebhookFilter.Regex.apply)
    }
    val not =
      c
        .downField("not")
        .success
        .map(
          _.as[WebhookFilter](webhookFilterDecoder)
            .map(WebhookFilter.Not(_))
        )

    equals orElse in orElse regex orElse not getOrElse
      DecodingFailure(s"Failed to match a webhook filter on $c", Nil).asLeft

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
    for
      min     <- c.downField("size").downField("min").as[Option[Int]]
      max     <- c.downField("size").downField("max").as[Option[Int]]
      message <- c.downField("message").as[Option[String]]
    yield Validation.Size(min, max, message)

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
    c.downField("in").success.map(_.as[NonEmptyList[String]].map[Validation](Validation.ContainedIn.apply)) orElse
      c.downField("enabledMarks").success.map(_.as[Set[String]].map[Validation](Validation.RichTextMarks.apply)) orElse
      c
        .downField("enabledNodeTypes")
        .success
        .map(_.as[Set[String]].map[Validation](Validation.RichTextNodeTypes.apply)) orElse
      c.downField("unique").success.map(_.as[Boolean].as[Validation](Validation.Unique)) orElse
      c.downField("validUrl").success.map(_.as[Boolean].as[Validation](Validation.Regexp.Url)) orElse
      c.downField("regexp").success.map(_.downField("pattern").as[String].map[Validation](Validation.Regexp.of(_))) orElse
      c.downField("size").success.as(c.as[Validation.Size]) orElse
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
    case Validation.RichTextMarks(enabledMarks) =>
      obj("enabledMarks" -> enabledMarks.asJson)
    case Validation.RichTextNodeTypes(enabledNodeTypes) =>
      obj("enabledNodeTypes" -> enabledNodeTypes.asJson)
    case Validation.Unique =>
      obj("unique" -> true.asJson)
    case Validation.Regexp(regexp) =>
      obj(
        "regexp" -> obj(
          "pattern" -> regexp.toString().asJson
        )
      )
    case Validation.Size(min, max, message) =>
      obj(
        "size" -> obj(
          "min" -> min.asJson,
          "max" -> max.asJson
        ),
        "message" -> message.asJson
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
