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

package io.dantb.contentless.graphql

import cats.parse.Parser
import io.dantb.contentless.{ContentTypeId, FieldType}

type GenerationResult[A] = Either[GenerationError, A]

enum GenerationError extends Throwable:
  case InvalidGraphQLDocument(e: Parser.Error)
  case MissingContentType(id: ContentTypeId)
  case EmptyListOfReferences(fieldId: String)
  case UnsupportedArrayType(fieldType: FieldType, fieldId: String)

// TODO: Locale should be an enum
final case class CollectionArguments(preview: Boolean, limit: Int, skip: Int, locale: String)
object CollectionArguments:
  val Default = CollectionArguments(false, 10, 0, "en-GB")
