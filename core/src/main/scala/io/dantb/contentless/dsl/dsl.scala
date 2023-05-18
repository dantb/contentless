package io.dantb.contentless

import io.dantb.contentless.codecs.{EntryCodec, FieldCodec}
import io.dantb.contentless.codecs.FieldCodec.Dsl

trait dsl extends Dsl

object dsl extends dsl:

  trait ContentType[A]:
    def contentType: ContentTypeId
    def contentTypeName: String
    def displayField: Option[String]
    def description: Option[String]
    def codec: EntryCodec[A]

  def contentType[A](
      contentType0: ContentTypeId,
      contentTypeName0: String,
      displayField0: Option[String],
      description0: Option[String],
      codec0: EntryCodec[A]
  ): ContentType[A] = new ContentType[A]:
    def contentType: ContentTypeId   = contentType0
    def contentTypeName: String      = contentTypeName0
    def displayField: Option[String] = displayField0
    def description: Option[String]  = description0
    def codec: EntryCodec[A]         = codec0
