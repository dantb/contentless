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
import cats.syntax.all.*
import edu.gemini.grackle.{GraphQLParser, Problem, QueryMinimizer}
import edu.gemini.grackle.Ast.*
import edu.gemini.grackle.Value.*
import io.dantb.contentless.*
import io.dantb.contentless.FieldType as FT
import io.dantb.contentless.FieldType.*
import io.dantb.contentless.dsl.*

def singleEntryQueryString[A: ContentType](id: String, model: ContentModel): GenerationResult[String] =
  singleEntryDoc(id, model).map(QueryMinimizer.minimizeDocument)

// TODO
// - functions to omit particular fields and include only specified fields.
// - function for one entry using a provided / derived decoder
// - function for all entries using provided / derived decoder
def contentTypeQueryString[A: ContentType](
    model: ContentModel = ContentModel.Empty
): GenerationResult[String] = contentTypeDoc(model).map(QueryMinimizer.minimizeDocument)

def contentTypeDoc[A: ContentType](
    model: ContentModel
): GenerationResult[Document] = generateDocumentWithArgs(model, CollectionArguments.Default)

def generateDocumentWithArgs[A: ContentType](
    model: ContentModel,
    arguments: CollectionArguments
): GenerationResult[Document] =
  codecFieldsSubqueries(model).map { ss =>
    val fields: List[Selection.Field] = sysField :: ss
    List(
      OperationDefinition.QueryShorthand(
        List(
          Selection.Field(
            None,
            Name(s"${ContentType[A].id.asString}Collection"),
            List(
              Name("skip")    -> Value.IntValue(arguments.skip),
              Name("limit")   -> Value.IntValue(arguments.limit),
              Name("preview") -> Value.BooleanValue(arguments.preview),
              Name("locale")  -> Value.StringValue(arguments.locale)
            ),
            Nil,
            List(field("items")(fields*))
          )
        )
      )
    )
  }

def singleEntryDoc[A: ContentType](
    id: String,
    model: ContentModel
): GenerationResult[Document] =
  codecFieldsSubqueries(model).map { ss =>
    val fields: List[Selection.Field] = sysField :: ss
    List(
      OperationDefinition.QueryShorthand(
        List(
          Selection.Field(
            None,
            Name(s"${ContentType[A].id.asString}"),
            List(Name("id") -> Value.StringValue(id)),
            Nil,
            fields
          )
        )
      )
    )
  }

def codecFieldsSubqueries[A: ContentType](
    model: ContentModel
): GenerationResult[List[Selection.Field]] =
  ContentType[A].codec.schema.sortBy(_.id).traverse(field => subQueryFromField(field, model))

def subQueryFromField(f: Field, model: ContentModel): GenerationResult[Selection.Field] =
  f.fieldType match
    case _: Text                        => field(f.id).asRight
    case _: FT.Media                    => imageField(f.id).asRight
    case FT.Reference(linkContentTypes) => contentfulReferenceSelection(linkContentTypes, f.id, model)
    case FT.Array(itemType, _) =>
      contentfulReferenceArraySelection(itemType, f.id, model)
    case _: RichText => richTextField(f.id).asRight
    case _: Integer  => field(f.id).asRight
    case _: Number   => field(f.id).asRight
    case FT.Boolean  => field(f.id).asRight
    case _: Json     => field(f.id).asRight
    case _: DateTime => field(f.id).asRight
    case FT.Location => locationField(f.id).asRight

def contentfulReferenceSelection(
    linkContentTypes: Set[ContentTypeId],
    fieldId: String,
    model: ContentModel
): GenerationResult[Selection.Field] =
  extractReferenceSubQueries(
    linkContentTypes,
    fieldId,
    model,
    groupFromField(fieldId, _, model)
  )

// TODO: this assumes only one content type can be referenced. In the case of multiple, we need the "..." syntax.
def extractReferenceSubQueries(
    linkContentTypes: Set[ContentTypeId],
    fieldId: String,
    model: ContentModel,
    subQueriesForContentType: ContentType[?] => GenerationResult[Selection.Field]
): GenerationResult[Selection.Field] =
  linkContentTypes.headOption
    .map(ct =>
      model.types
        .collectFirst {
          case c if c.id === ct => subQueriesForContentType(c)
        }
        .getOrElse(Left(GenerationError.MissingContentType(ct)))
    )
    .getOrElse(
      Left(GenerationError.EmptyListOfReferences(fieldId))
    )

def groupFromField(fieldId: String, ct: ContentType[?], model: ContentModel): GenerationResult[Selection.Field] =
  val subQueries = ct.codec.schema.sortBy(_.id).traverse(subQueryFromField(_, model))
  subQueries.map(subQueries => field(fieldId)((sysField :: subQueries)*))

// TODO: currently only supports arrays of references - handle primitive arrays too
def contentfulReferenceArraySelection(
    itemType: FieldType,
    fieldId: String,
    model: ContentModel
): GenerationResult[Selection.Field] =
  itemType match
    case io.dantb.contentless.FieldType.Reference(linkContentTypes) =>
      extractReferenceSubQueries(
        linkContentTypes,
        fieldId,
        model,
        groupFromField("items", _, model).map(fields => field(fieldId ++ "Collection")(fields))
      )
    // only 'symbol' (short text fields) are supported in arrays
    case io.dantb.contentless.FieldType.Text(false, _, _, _, _) => field(fieldId).asRight
    case other => Left(GenerationError.UnsupportedArrayType(other, fieldId))

def field(id: String)(subFields: Selection.Field*): Selection.Field =
  Selection.Field(None, Name(id), List(), List(), subFields.toList)
def field(id: String): Selection.Field = Selection.Field(None, Name(id), List(), List(), Nil)

def imageField(fieldId: String): Selection.Field =
  field(fieldId)(
    sysField,
    field("description"),
    field("height"),
    field("title"),
    field("url"),
    field("width")
  )

val sysField: Selection.Field =
  field("sys")(
    field("id"),
    field("publishedAt"),
    field("firstPublishedAt"),
    field("publishedVersion")
  )

def locationField(fieldId: String): Selection.Field = field(fieldId)(field("lat"), field("lon"))

def richTextField(fieldId: String): Selection.Field = field(fieldId)(field("json"))
