// /*
//  * Copyright 2023 Typelevel
//  *
//  * Licensed under the Apache License, Version 2.0 (the "License");
//  * you may not use this file except in compliance with the License.
//  * You may obtain a copy of the License at
//  *
//  *     http://www.apache.org/licenses/LICENSE-2.0
//  *
//  * Unless required by applicable law or agreed to in writing, software
//  * distributed under the License is distributed on an "AS IS" BASIS,
//  * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//  * See the License for the specific language governing permissions and
//  * limitations under the License.
//  */

package io.dantb.contentless.codecs

import java.time.{LocalDateTime, ZoneId, ZonedDateTime}
import java.time.format.DateTimeFormatter
import java.time.temporal.ChronoUnit

import cats.data.NonEmptyList
import io.circe.Json
import io.circe.syntax.*
import io.dantb.contentless.{ContentModel, ContentTypeId, Field, FieldType, Media, MimeTypeGroup, RichTextNodeType, Validation}
import io.dantb.contentless.RichText.Mark
import io.dantb.contentless.Validation.RichTextNodes
import io.dantb.contentless.arbitrary.given
import io.dantb.contentless.codecs.FieldCodec.DefaultZone
import io.dantb.contentless.codecs.implicits.given
import io.dantb.contentless.dsl.*
import io.dantb.contentless.instances.given
import munit.ScalaCheckSuite
import org.scalacheck.Prop.*

class EntryCodecSpec extends ScalaCheckSuite:

  property("bool field") {
    forAll { (id: String, name: String, disabled: Boolean, default: Option[Boolean]) =>
      val fieldCodec = boolean(id, name, default)

      assertField(fieldCodec, id, name, disabled, FieldType.Boolean, default.map(_.asJson))
    }
  }

  property("text field") {
    forAll { (id: String, name: String, disabled: Boolean, default: Option[String]) =>
      val fieldCodec        = text(id, name, defaultValue = default)
      val expectedFieldType = FieldType.Text(longText = false, None, None, None)

      assertField(fieldCodec, id, name, disabled, expectedFieldType, default.map(_.asJson))
    }
  }

  property("long text field") {
    forAll { (id: String, name: String, disabled: Boolean, default: Option[String]) =>
      val fieldCodec        = longText(id, name, defaultValue = default)
      val expectedFieldType = FieldType.Text(longText = true, None, None, None)

      assertField(fieldCodec, id, name, disabled, expectedFieldType, default.map(_.asJson))
    }
  }

  property("int field") {
    forAll {
      (id: String, name: String, disabled: Boolean, default: Option[Int], allowedValues: Option[NonEmptyList[Int]]) =>
        val fieldCodec        = int(id, name, default, allowedValues)
        val expectedFieldType = FieldType.Integer(allowedValues)

        assertField(fieldCodec, id, name, disabled, expectedFieldType, default.map(_.asJson))
    }
  }

  property("decimal field") {
    forAll {
      (id: String, name: String, disabled: Boolean, default: Option[Double], allowedValues: Option[NonEmptyList[Double]]) =>
        val fieldCodec        = decimal(id, name, default, allowedValues)
        val expectedFieldType = FieldType.Number(allowedValues)

        assertField(fieldCodec, id, name, disabled, expectedFieldType, default.map(_.asJson))
    }
  }

  property("json field") {
    forAll { (id: String, name: String, disabled: Boolean, minProperties: Option[Int], maxProperties: Option[Int]) =>
      val fieldCodec        = json[Int](id, name, minProperties, maxProperties)
      val expectedFieldType = FieldType.Json(minProperties, maxProperties)

      assertField(fieldCodec, id, name, disabled, expectedFieldType, None)
    }
  }

  property("local date time field") {
    forAll {
      (
          id: String,
          name: String,
          disabled: Boolean,
          default: Option[LocalDateTime],
          minDate: Option[LocalDateTime],
          maxDate: Option[LocalDateTime]
      ) =>
        val fieldCodec        = dateTime(id, name, default, minDate, maxDate)
        val expectedFieldType = FieldType.DateTime(minDate.map(_.atZone(DefaultZone)), maxDate.map(_.atZone(DefaultZone)))

        assertField(fieldCodec, id, name, disabled, expectedFieldType, default.map(_.asJson))
    }
  }

  property("zoned date time field") {
    forAll {
      (
          id: String,
          name: String,
          disabled: Boolean,
          default: Option[ZonedDateTime],
          minDate: Option[ZonedDateTime],
          maxDate: Option[ZonedDateTime]
      ) =>
        val fieldCodec        = zonedDateTime(id, name, default, minDate, maxDate)
        val expectedFieldType = FieldType.DateTime(minDate, maxDate)
        val formattedTruncatedDefault =
          default.map(x =>
            x.withZoneSameInstant(ZoneId.of("UTC"))
              .truncatedTo(ChronoUnit.MILLIS)
              .format(DateTimeFormatter.ISO_INSTANT)
              .asJson
          )

        assertField(fieldCodec, id, name, disabled, expectedFieldType, formattedTruncatedDefault)
    }
  }

  property("location field") {
    forAll { (id: String, name: String, disabled: Boolean) =>
      val fieldCodec        = location(id, name)
      val expectedFieldType = FieldType.Location

      assertField(fieldCodec, id, name, disabled, expectedFieldType, None)
    }
  }

  property("rich text field") {
    forAll {
      (
          id: String,
          name: String,
          disabled: Boolean,
          allowedNodeTypes: Set[RichTextNodeType],
          allowedMarks: Set[Mark],
          entryHyperlink: Option[RichTextNodes.EntryHyperlink],
          entryBlock: Option[RichTextNodes.EntryBlock],
          entryInline: Option[RichTextNodes.EntryInline],
          assetHyperlinkSize: Option[Validation.Size],
          assetBlockSize: Option[Validation.Size]
      ) =>
        val fieldCodec = richText(
          id,
          name,
          allowedNodeTypes,
          allowedMarks,
          entryHyperlink,
          entryBlock,
          entryInline,
          assetHyperlinkSize,
          assetBlockSize
        )

        val expectedFieldType = FieldType.RichText(
          allowedNodeTypes,
          allowedMarks,
          entryHyperlink,
          entryBlock,
          entryInline,
          assetHyperlinkSize,
          assetBlockSize
        )

        assertField(fieldCodec, id, name, disabled, expectedFieldType, None)
    }
  }

  property("text list field") {
    forAll { (id: String, name: String, disabled: Boolean, default: Option[List[String]]) =>
      val fieldCodec        = textList(id, name, defaultValue = default)
      val expectedFieldType = FieldType.Array(FieldType.Text(longText = false, None, None, None), None)

      assertField(fieldCodec, id, name, disabled, expectedFieldType, default.map(_.asJson))
    }
  }

  property("asset field") {
    forAll { (id: String, name: String, disabled: Boolean, mimeTypes: Set[MimeTypeGroup]) =>
      val fieldCodec        = asset(id, name, mimeTypes)
      val expectedFieldType = FieldType.Media(mimeTypes)

      assertField[Media](fieldCodec, id, name, disabled, expectedFieldType, None)
    }
  }

  property("assets field") {
    forAll { (id: String, name: String, disabled: Boolean, mimeTypes: Set[MimeTypeGroup]) =>
      val fieldCodec        = assets(id, name, mimeTypes)
      val expectedFieldType = FieldType.Array(FieldType.Media(mimeTypes), None)

      assertField(fieldCodec, id, name, disabled, expectedFieldType, None)
    }
  }

  property("entry field") {
    forAll { (id: String, name: String, disabled: Boolean, contentTypes: Set[ContentTypeId]) =>
      val fieldCodec        = entry(id, name, contentTypes)
      val expectedFieldType = FieldType.Reference(contentTypes)

      assertField(fieldCodec, id, name, disabled, expectedFieldType, None)
    }
  }

  property("entries field") {
    forAll { (id: String, name: String, disabled: Boolean, contentTypes: Set[ContentTypeId]) =>
      val fieldCodec        = entries(id, name, contentTypes)
      val expectedFieldType = FieldType.Array(FieldType.Reference(contentTypes), None)

      assertField(fieldCodec, id, name, disabled, expectedFieldType, None)
    }
  }

  def assertField[A](
      fieldCodec: FieldCodec[A],
      id: String,
      name: String,
      disabled: Boolean,
      expectedFieldType: FieldType,
      expectedDefaultJson: Option[Json]
  ): Unit =
    val fieldCodecWithDisabled = withDisabled(fieldCodec, disabled)
    val expectedDefaultValue =
      expectedDefaultJson.map(d => Map(FieldCodec.DefaultLocale.code -> d.asJson)).getOrElse(Map.empty)
    val expected = Field(id, name, required = true, disabled = disabled, expectedFieldType, expectedDefaultValue)

    assertEquals(
      fieldCodecWithDisabled.required.schema.headOption,
      Some(expected)
    )
    assertEquals(
      fieldCodecWithDisabled.optional.schema.headOption,
      Some(expected.copy(required = false))
    )

  def withDisabled[A](fieldCodec: FieldCodec[A], disabled: Boolean): FieldCodec[A] =
    if disabled then fieldCodec.disabled else fieldCodec

  // test("basic content type with primitives") {
  //   final case class Foo(t: String, i: Int, b: Boolean, d: Double, lt: Option[String])

  //   val fooContentType: ContentType[Foo] =
  //     ContentType(
  //       ContentTypeId("foo"),
  //       "Foo",
  //       Some("t"),
  //       Some("A foo does foo things"),
  //       (text("t", "Text field").required *:
  //         int("i", "Integer field").required *:
  //         boolean("b", "Boolean field").required *:
  //         decimal("d", "Decimal field").required *:
  //         longText("lt", "Optional long text field").optional).to[Foo]
  //     )

  //   assertEquals(
  //     fooContentType.codec.schema,
  //     List()
  //   )
  // }
