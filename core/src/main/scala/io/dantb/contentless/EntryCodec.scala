package io.dantb.contentless

import java.time.{LocalDateTime, ZonedDateTime}

import cats.InvariantMonoidal
import cats.syntax.all.*
import io.circe.{Decoder, Encoder, Json}
import io.circe.syntax.*
import io.dantb.contentless.RichText.Node
import io.dantb.contentless.appearance.PrettyEntryCodec
import io.dantb.contentless.circe.implicits.given

sealed trait EntryCodec[A]:
  self =>
  def schema: List[Field]
  def read(input: Map[String, Json]): Either[String, A]
  def write(value: A): Map[String, Json]

  final def imap[B](f: A => B)(g: B => A): EntryCodec[B] =
    eimap(f(_).asRight)(g)

  final def eimap[B](f: A => Either[String, B])(g: B => A): EntryCodec[B] =
    new EntryCodec[B]:
      override def read(input: Map[String, Json]): Either[String, B] = self.read(input).flatMap(f)
      override def write(value: B): Map[String, Json]                = self.write(g(value))
      override val schema: List[Field]                               = self.schema

  final def product[B](other: EntryCodec[B]): EntryCodec[(A, B)] =
    new EntryCodec[(A, B)]:
      override def read(input: Map[String, Json]): Either[String, (A, B)] = self.read(input).product(other.read(input))
      override def write(value: (A, B)): Map[String, Json]                = self.write(value._1) ++ other.write(value._2)
      override val schema: List[Field]                                    = self.schema ++ other.schema

object EntryCodec:
  def apply[A](using codec: EntryCodec[A]): EntryCodec[A] = codec

  def unit: EntryCodec[Unit] =
    new EntryCodec[Unit]:
      override def read(input: Map[String, Json]): Either[String, Unit] = Right(())
      override def write(value: Unit): Map[String, Json]                = Map.empty
      override val schema: List[Field]                                  = Nil

  implicit def invariantMonoidal: InvariantMonoidal[EntryCodec] =
    new InvariantMonoidal[EntryCodec]:
      override def unit: EntryCodec[Unit] = EntryCodec.unit
      override def product[A, B](fa: EntryCodec[A], fb: EntryCodec[B]): EntryCodec[(A, B)] =
        fa.product(fb)
      override def imap[A, B](fa: EntryCodec[A])(f: A => B)(g: B => A): EntryCodec[B] = fa.imap(f)(g)

  implicit def fromPrettyEntryCodec[A](using pretty: PrettyEntryCodec[A]): EntryCodec[A] =
    pretty.entryCodec

final case class FieldCodec[A] private (
    fieldId: String,
    fieldType: FieldType,
    fieldName: String,
    locale: Locale,
    isDisabled: Boolean,
    defaultValue: Option[A]
)(using encoder: Encoder[A], decoder: Decoder[A]):
  def required: EntryCodec[A] =
    new EntryCodec[A]:
      override def read(fields: Map[String, Json]): Either[String, A] =
        fields
          .get(fieldId)
          .toRight(s"Field not found: $fieldId ($fieldType)")
          .flatMap(_.as[Map[String, Json]].leftMap(_.toString()))
          .flatMap(_.get(locale.code).toRight(s"Localization not found for ${locale.code}"))
          .flatMap(_.as[A].leftMap(_.toString()))
          .leftMap(err => s"$err occurred when parsing $fieldId of type $fieldType")
      override def write(value: A): Map[String, Json] =
        val localized = Map(locale.code -> value.asJson).asJson
        Map(fieldId -> localized)
      override def schema: List[Field] =
        List(
          Field(
            fieldId,
            fieldName,
            required = true,
            isDisabled,
            fieldType,
            defaultValue.map(value => Map(locale.code -> value.asJson)).getOrElse(Map.empty)
          )
        )

  def optional: EntryCodec[Option[A]] =
    new EntryCodec[Option[A]]:
      override def read(fields: Map[String, Json]): Either[String, Option[A]] =
        fields
          .get(fieldId)
          .traverse[Either[String, *], A] {
            _.as[Map[String, Json]]
              .leftMap(_.toString())
              .flatMap(_.get(locale.code).toRight(s"Localization not found for ${locale.code}"))
              .flatMap(_.as[A].leftMap(_.toString()))
          }
          .leftMap(err => s"$err occurred when parsing $fieldId of type $fieldType")

      override def write(option: Option[A]): Map[String, Json] =
        option match
          case Some(value) =>
            val localized = Map(locale.code -> value.asJson).asJson
            Map(fieldId -> localized)
          case None => Map.empty

      override def schema: List[Field] =
        List(
          Field(
            fieldId,
            fieldName,
            required = false,
            isDisabled,
            fieldType,
            defaultValue.map(value => Map(locale.code -> value.asJson)).getOrElse(Map.empty)
          )
        )

  def withLocale(newLocale: Locale): FieldCodec[A] = this.copy(locale = newLocale)
  def disabled: FieldCodec[A]                      = this.copy(isDisabled = true)

private[contentless] object FieldCodec:
  val defaultLocale: Locale = Locale.enGB

  def apply[A: Encoder: Decoder](
      fieldId: String,
      fieldType: FieldType,
      fieldName: String,
      defaultValue: Option[A]
  ): FieldCodec[A] = FieldCodec(fieldId, fieldType, fieldName, defaultLocale, isDisabled = false, defaultValue)

  trait Dsl:
    def text(
        fieldId: String,
        fieldName: String,
        validations: Set[Validation] = Set.empty,
        longText: Boolean = false,
        defaultValue: Option[String] = None
    ): FieldCodec[String] =
      FieldCodec(fieldId, FieldType.Text(longText, validations), fieldName, defaultValue)

    def longText(
        fieldId: String,
        fieldName: String,
        validations: Set[Validation] = Set.empty,
        defaultValue: Option[String] = None
    ): FieldCodec[String] =
      FieldCodec(fieldId, FieldType.Text(longText = true, validations), fieldName, defaultValue)

    def textList(
        fieldId: String,
        fieldName: String,
        validations: Set[Validation] = Set.empty,
        minLength: Option[Int] = None,
        maxLength: Option[Int] = None,
        longText: Boolean = false,
        defaultValue: Option[List[String]] = None
    ): FieldCodec[List[String]] =
      FieldCodec(
        fieldId,
        FieldType.Array(FieldType.Text(longText, validations), minLength, maxLength),
        fieldName,
        defaultValue
      )

    def richText(fieldId: String, fieldName: String, validations: Set[Validation] = Set.empty): FieldCodec[Node] =
      FieldCodec(fieldId, FieldType.RichText(validations), fieldName, None)

    def media(fieldId: String, fieldName: String, mimeTypeGroup: Set[MimeTypeGroup]): FieldCodec[Media] =
      FieldCodec(fieldId, FieldType.Media(mimeTypeGroup), fieldName, None)

    def reference(fieldId: String, fieldName: String, linkContentTypes: Set[ContentTypeId]): FieldCodec[Reference] =
      FieldCodec(fieldId, FieldType.Reference(linkContentTypes), fieldName, None)

    def references(
        fieldId: String,
        fieldName: String,
        linkContentTypes: Set[ContentTypeId],
        minLength: Option[Int] = None,
        maxLength: Option[Int] = None
    ): FieldCodec[List[Reference]] =
      FieldCodec(
        fieldId,
        FieldType.Array(FieldType.Reference(linkContentTypes), minLength, maxLength),
        fieldName,
        None
      )

    def boolean(fieldId: String, fieldName: String, defaultValue: Option[Boolean] = None): FieldCodec[Boolean] =
      FieldCodec(fieldId, FieldType.Boolean, fieldName, defaultValue)

    def integer(fieldId: String, fieldName: String, defaultValue: Option[Int] = None): FieldCodec[Int] =
      FieldCodec(fieldId, FieldType.Integer, fieldName, defaultValue)

    def decimal(fieldId: String, fieldName: String, defaultValue: Option[Double] = None): FieldCodec[Double] =
      FieldCodec(fieldId, FieldType.Number, fieldName, defaultValue)

    def location(fieldId: String, fieldName: String): FieldCodec[Location] =
      FieldCodec(fieldId, FieldType.Location, fieldName, None)

    def json[A: Encoder: Decoder](fieldId: String, fieldName: String): FieldCodec[A] =
      FieldCodec(fieldId, FieldType.Json, fieldName, None)

    def dateTime(
        fieldId: String,
        fieldName: String,
        defaultValue: Option[LocalDateTime] = None
    ): FieldCodec[LocalDateTime] =
      FieldCodec(fieldId, FieldType.DateTime, fieldName, defaultValue)

    def zonedDateTime(
        fieldId: String,
        fieldName: String,
        defaultValue: Option[ZonedDateTime] = None
    ): FieldCodec[ZonedDateTime] =
      FieldCodec(fieldId, FieldType.DateTime, fieldName, defaultValue)
