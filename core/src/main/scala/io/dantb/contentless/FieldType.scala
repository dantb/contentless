package io.dantb.contentless

import java.time.ZonedDateTime

import cats.Eq
import cats.data.NonEmptyList
import cats.syntax.all.*
import io.dantb.contentless.RichText.Mark
import io.dantb.contentless.Validation.{Regexp, RegexpValidation, RichTextNodes}

import scala.util.matching.Regex

sealed trait FieldType

object FieldType:

  given eq: Eq[FieldType] = Eq.fromUniversalEquals

  final case class Text(
      longText: Boolean,
      charBounds: Option[Validation.Size],
      allowedValues: Option[NonEmptyList[String]],
      matchesRegex: Option[RegexpValidation],
      unique: Boolean
  ) extends FieldType:
    def validations: Set[Validation] =
      charBounds.toSet ++ matchesRegex.toSet ++ allowedValues.map(Validation.ContainedIn(_)).toSet ++
        (if unique then Set(Validation.Unique) else Set())
  object Text:
    def fromValidations(longText: Boolean, vs: Set[Validation]): Text =
      val size          = vs.collectFirst { case s: Validation.Size => s }
      val unique        = vs.collectFirst { case s: Validation.Unique.type => true }.getOrElse(false)
      val allowedValues = vs.collectFirst { case s: Validation.ContainedIn => s }.map(_.allowedValues)
      val matchesRegex  = vs.collectFirst { case s: Validation.RegexpValidation => s }
      Text(longText, size, allowedValues, matchesRegex, unique)

  final case class Media(mimeTypeGroup: Set[MimeTypeGroup]) extends FieldType

  final case class Reference(linkContentTypes: Set[ContentTypeId]) extends FieldType

  final case class Array(itemType: FieldType, arrayBounds: Option[Validation.Size]) extends FieldType:
    def validations: Set[Validation] = arrayBounds.toSet

  final case class RichText(
      allowedNodeTypes: Set[RichTextNodeType],
      allowedMarks: Set[Mark],
      entryHyperlink: Option[RichTextNodes.EntryHyperlink],
      entryBlock: Option[RichTextNodes.EntryBlock],
      entryInline: Option[RichTextNodes.EntryInline],
      assetHyperlinkSize: Option[Validation.Size],
      assetBlockSize: Option[Validation.Size]
  ) extends FieldType:
    def validations: Set[Validation] = Set(
      Validation.RichTextNodeTypes(allowedNodeTypes),
      Validation.RichTextMarks(allowedMarks),
      Validation.RichTextNodes(assetHyperlinkSize, entryHyperlink, assetBlockSize, entryBlock, entryInline)
    )

  object RichText:
    def fromValidations(vs: Set[Validation]): RichText =
      val allowedNodeTypes = vs.collectFirst { case Validation.RichTextNodeTypes(n) => n }.getOrElse(Set.empty)
      val allowedMarks     = vs.collectFirst { case Validation.RichTextMarks(n) => n }.getOrElse(Set.empty)
      val nodes            = vs.collectFirst { case s: Validation.RichTextNodes => s }
      RichText(
        allowedNodeTypes,
        allowedMarks,
        nodes.flatMap(_.entryHyperlink),
        nodes.flatMap(_.entryBlock),
        nodes.flatMap(_.entryInline),
        nodes.flatMap(_.assetHyperlinkSize),
        nodes.flatMap(_.assetBlockSize)
      )

  final case class Integer(allowedValues: Option[NonEmptyList[Int]], range: Option[Validation.Size], unique: Boolean)
      extends FieldType:
    def validations: Set[Validation] = allowedValues.map(Validation.ContainedInInt(_)).toSet
  object Integer:
    def fromValidations(vs: Set[Validation]): Integer =
      val range         = vs.collectFirst { case s: Validation.Size => s }
      val unique        = vs.collectFirst { case s: Validation.Unique.type => true }.getOrElse(false)
      val allowedValues = vs.collectFirst { case s: Validation.ContainedInInt => s }.map(_.allowedValues)
      Integer(allowedValues, range, unique)

  final case class Number(allowedValues: Option[NonEmptyList[Double]], unique: Boolean) extends FieldType:
    def validations: Set[Validation] = allowedValues.map(Validation.ContainedInDecimal(_)).toSet
  object Number:
    def fromValidations(vs: Set[Validation]): Number =
      val unique        = vs.collectFirst { case s: Validation.Unique.type => true }.getOrElse(false)
      val allowedValues = vs.collectFirst { case s: Validation.ContainedInDecimal => s }.map(_.allowedValues)
      Number(allowedValues, unique)

  case object Boolean extends FieldType

  final case class Json(minProperties: Option[Int], maxProperties: Option[Int]) extends FieldType:
    def validations: Set[Validation] = Set(Validation.Size(minProperties, maxProperties, None, "size"))

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
