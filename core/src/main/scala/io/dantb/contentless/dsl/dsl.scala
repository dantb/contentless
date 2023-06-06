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

import io.dantb.contentless.codecs.{EntryCodec, FieldCodec}
import io.dantb.contentless.codecs.FieldCodec.Dsl

trait dsl extends Dsl

object dsl extends dsl:

  final case class ContentModel(
      types: List[ContentType[?]]
  )

  final case class ContentTpe(
      id: ContentTypeId,
      displayName: String,
      displayField: Option[String],
      description: Option[String],
      fields: List[Field]
  )

  object ContentModel:
    val Empty = ContentModel(Nil)
    def of(contentTypes: ContentType[?]*): ContentModel =
      ContentModel(contentTypes.toList)

  trait ContentType[A]:
    def id: ContentTypeId
    def displayName: String
    def displayField: Option[String]
    def description: Option[String]
    def codec: EntryCodec[A]

  object ContentType:
    def apply[A](using ct: ContentType[A]) = ct

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
