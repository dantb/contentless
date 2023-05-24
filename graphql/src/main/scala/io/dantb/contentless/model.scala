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
