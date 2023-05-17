package io.dantb.contentless

import java.time.ZonedDateTime

import cats.Eq
import cats.data.NonEmptyList
import cats.syntax.all.*
import io.dantb.contentless.Validation.Regexp

import scala.util.matching.Regex

sealed trait FieldType

object FieldType:

  given eq: Eq[FieldType] = Eq.instance {
    case (a: Text, b: Text) =>
      a.longText === b.longText && a.minChars === b.minChars && a.maxChars === b.maxChars && a.allowedValues === b.allowedValues && a.matchesRegex === b.matchesRegex
    case (a: RichText, b: RichText) => a.validations === b.validations
    case (a: Array, b: Array)       =>
      // using the `val eq` explicitly here, since implicitly using
      // the val while inside the closure is now considered dangerous and incurs a compile-error under -Wself-implicit
      eq.eqv(a.itemType, b.itemType) && a.minLength === b.minLength && a.maxLength === b.maxLength
    case (a, b) => a == b
  }

  final case class Text(
      longText: Boolean,
      minChars: Option[Int],
      maxChars: Option[Int],
      allowedValues: Option[NonEmptyList[String]],
      matchesRegex: Option[Regexp]
  ) extends FieldType:
    def validations: Set[Validation] = Set(Validation.Size(minChars, maxChars, None)) ++ matchesRegex.toSet ++ allowedValues
      .map(Validation.ContainedIn(_))
      .toSet
  object Text:
    def fromValidations(longText: Boolean, vs: Set[Validation]): Text =
      val size          = vs.collectFirst { case s: Validation.Size => s }
      val allowedValues = vs.collectFirst { case s: Validation.ContainedIn[String] => s }.map(_.allowedValues)
      val matchesRegex  = vs.collectFirst { case s: Validation.Regexp => s }
      Text(longText, size.flatMap(_.min), size.flatMap(_.max), allowedValues, matchesRegex)

  final case class Media(mimeTypeGroup: Set[MimeTypeGroup]) extends FieldType

  final case class Reference(linkContentTypes: Set[ContentTypeId]) extends FieldType

  final case class Array(itemType: FieldType, minLength: Option[Int], maxLength: Option[Int]) extends FieldType

  final case class RichText(validations: Set[Validation]) extends FieldType

  final case class Integer(allowedValues: Option[NonEmptyList[Int]]) extends FieldType:
    def validations: Set[Validation] = allowedValues.map(Validation.ContainedIn(_)).toSet
  object Integer:
    def fromValidations(vs: Set[Validation]): Integer =
      val allowedValues = vs.collectFirst { case s: Validation.ContainedIn[Int] => s }.map(_.allowedValues)
      Integer(allowedValues)

  final case class Number(allowedValues: Option[NonEmptyList[Double]]) extends FieldType:
    def validations: Set[Validation] = allowedValues.map(Validation.ContainedIn(_)).toSet
  object Number:
    def fromValidations(vs: Set[Validation]): Number =
      val allowedValues = vs.collectFirst { case s: Validation.ContainedIn[Double] => s }.map(_.allowedValues)
      Number(allowedValues)

  case object Boolean extends FieldType

  final case class Json(minProperties: Option[Int], maxProperties: Option[Int]) extends FieldType:
    def validations: Set[Validation] = Set(Validation.Size(minProperties, maxProperties, None))

  object Json:
    def fromValidations(vs: Set[Validation]): Json =
      val size = vs.collectFirst { case s: Validation.Size => s }
      Json(size.flatMap(_.min), size.flatMap(_.max))

  final case class DateTime(minDate: Option[ZonedDateTime], maxDate: Option[ZonedDateTime]) extends FieldType:
    def validations: Set[Validation] = Set(Validation.DateRange(minDate, maxDate))
  object DateTime:
    def fromValidations(vs: Set[Validation]): DateTime =
      val size = vs.collectFirst { case s: Validation.DateRange => s }
      DateTime(size.flatMap(_.min), size.flatMap(_.max))

  case object Location extends FieldType
