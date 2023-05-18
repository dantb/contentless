package io.dantb.contentless

import io.dantb.contentless.codecs.{EntryCodec, FieldCodec}
import io.dantb.contentless.codecs.FieldCodec.Dsl

trait dsl extends Dsl

object dsl extends dsl:

  trait ContentType[A]:
    def id: ContentTypeId
    def displayName: String
    def displayField: Option[String]
    def description: Option[String]
    def codec: EntryCodec[A]

  def contentType[A](
      id0: ContentTypeId,
      displayName0: String,
      displayField0: Option[String],
      description0: Option[String],
      codec0: EntryCodec[A]
  ): ContentType[A] = new ContentType[A]:
    def id: ContentTypeId            = id0
    def displayName: String          = displayName0
    def displayField: Option[String] = displayField0
    def description: Option[String]  = description0
    def codec: EntryCodec[A]         = codec0
