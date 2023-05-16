package io.dantb.contentless

import io.dantb.contentless.codecs.{EntryCodec, FieldCodec}
import io.dantb.contentless.codecs.FieldCodec.Dsl

trait dsl extends Dsl

object dsl extends dsl:
  final case class ContentType[A](
      contentType: ContentTypeId,
      contentTypeName: String,
      displayField: Option[String],
      description: Option[String],
      codec: EntryCodec[A]
  )
