package io.dantb.contentless

import java.time.ZonedDateTime

import cats.{Eq, Show}
import cats.data.NonEmptyList
import cats.syntax.all.*
import io.circe.{Encoder, Json}
import io.dantb.contentless.RichText.Mark
import io.dantb.contentless.codecs.EntryCodec
import io.dantb.contentless.instances.given

import scala.util.matching.Regex

/** One of the core entities in Contentful - represents the core information for a [[ContentType]].
  *
  * While they CAN be omitted (set to None), we recommend adding human-friendly description of the type's purpose in life,
  * as well as a `displayField`, which informs contentful which field from the model should be used for display in lists,
  * searches, references, etc.
  *
  * See the Contentful-Docs for a bit more detail, if you wish.
  *
  * [[https://www.contentful.com/developers/docs/references/content-management-api/#/reference/content-types/content-type]]
  */
trait ContentModel[A]:

  /** The primary identifier in Contentful for this Content Model */
  def contentType: ContentTypeId

  /** The Human-Friendly name for this Content Model */
  def contentTypeName: String

  /** The field from the Model that will be used for displaying instances of this Content Model within contentful */
  def displayField: Option[String]

  /** The Human-Friendly description with the raison d'être of for instances of this Content Model */
  def description: Option[String]

  def codec: EntryCodec[A]

object ContentModel:
  def apply[A](using contentModel: ContentModel[A]): ContentModel[A] = contentModel

/** The ID of the content type. Must be unique (within each environment) */
final case class ContentTypeId(asString: String) extends AnyVal

object ContentTypeId:
  given eq: Eq[ContentTypeId]     = Eq.by(_.asString)
  given show: Show[ContentTypeId] = _.asString

  def of[A: ContentModel]: ContentTypeId =
    ContentModel[A].contentType

final case class Reference(id: String) extends AnyVal

final case class Location(latitude: Double, longitude: Double)

final case class Field(
    id: String,
    name: String,
    required: Boolean,
    disabled: Boolean,
    fieldType: FieldType,
    defaultValue: Map[String, Json]
)

object Field:
  given eq: Eq[Field] = Eq.instance { (a, b) =>
    a.id === b.id && a.name === b.name && a.required === b.required && a.disabled === b.disabled &&
    a.fieldType === b.fieldType && a.defaultValue === b.defaultValue
  }

// Include everything here: https://www.contentful.com/help/available-validations/
sealed trait Validation extends Product with Serializable

object Validation:
  given eq: Eq[Validation] = Eq.fromUniversalEquals

  final case class ContainedIn(allowedValues: NonEmptyList[String])        extends Validation
  final case class ContainedInInt(allowedValues: NonEmptyList[Int])        extends Validation
  final case class ContainedInDecimal(allowedValues: NonEmptyList[Double]) extends Validation

  final case class RichTextNodes(
      assetHyperlinkSize: Option[Size],
      entryHyperlink: Option[RichTextNodes.EntryHyperlink],
      assetBlockSize: Option[Size],
      entryBlock: Option[RichTextNodes.EntryBlock],
      entryInline: Option[RichTextNodes.EntryInline]
  ) extends Validation
  object RichTextNodes:
    def empty: RichTextNodes = RichTextNodes(None, None, None, None, None)

    final case class EntryInline(size: Option[Size], linkContentType: Option[LinkContentType])
    final case class EntryHyperlink(size: Option[Size], linkContentType: Option[LinkContentType])
    final case class EntryBlock(size: Option[Size], linkContentType: Option[LinkContentType])

  final case class LinkContentType(allowedContentTypes: Set[String], message: Option[String]) extends Validation

  final case class RichTextMarks(allowedValues: Set[Mark]) extends Validation:
    def message: Option[String] = allowedValuesMessage(allowedValues.map(_.asString), "marks")

  final case class RichTextNodeTypes(allowedValues: Set[RichTextNodeType]) extends Validation:
    def message: Option[String] = allowedValuesMessage(allowedValues.map(_.asString.replaceAll("-", " ")), "nodes")

  private def allowedValuesMessage(s: Set[String], tpe: "nodes" | "marks"): Option[String] = s.toList match
    case x :: Nil   => s"Only $x $tpe are allowed".some
    case xs :+ last => s"Only ${xs.mkString(", ")} and $last $tpe are allowed".some
    case _          => None

  final case class Size(min: Option[Int], max: Option[Int], message: Option[String], tpe: "size" | "range")
      extends Validation
  object Size:
    def range(min: Int, max: Int, message: String): Size = Size(min.some, max.some, message.some, "range")
    def size(min: Int, max: Int, message: String): Size  = Size(min.some, max.some, message.some, "size")

  final case class DateRange(min: Option[ZonedDateTime], max: Option[ZonedDateTime]) extends Validation
  case object Unique                                                                 extends Validation

  final case class RegexpValidation(regexp: Regexp, message: Option[String]) extends Validation

  enum Regexp(val underlying: Regex):
    case Url
        extends Regexp("^(ftp|http|https):\\/\\/(\\w+:{0,1}\\w*@)?(\\S+)(:[0-9]+)?(\\/|\\/([\\w#!:.?+=&%@!\\-\\/]))?$".r)
    case Email                  extends Regexp("^\\w[\\w.-]*@([\\w-]+\\.)+[\\w-]+$".r)
    case Slug                   extends Regexp("^[a-z0-9]+(?:-[a-z0-9]+)*$".r)
    case DateUS                 extends Regexp("^(0?[1-9]|1[012])[- /.](0?[1-9]|[12][0-9]|3[01])[- /.](19|20)?\\d\\d$".r)
    case DateEurope             extends Regexp("^(0?[1-9]|[12][0-9]|3[01])[- /.](0?[1-9]|1[012])[- /.](19|20)?\\d\\d$".r)
    case Time12H                extends Regexp("^(0?[1-9]|1[012]):[0-5][0-9](:[0-5][0-9])?\\s*[aApP][mM]$".r)
    case Time24H                extends Regexp("^(0?[0-9]|1[0-9]|2[0-3]):[0-5][0-9](:[0-5][0-9])?$".r)
    case PhoneNumUS             extends Regexp("^\\d[ -.]?\\(?\\d\\d\\d\\)?[ -.]?\\d\\d\\d[ -.]?\\d\\d\\d\\d$".r)
    case ZipCodeUS              extends Regexp("^\\d{5}$|^\\d{5}-\\d{4}$}".r)
    case Custom(pattern: Regex) extends Regexp(pattern)

  object Regexp:
    def unapply(r: Regexp): Some[Regex] = Some(r.underlying)

    import Regexp.*
    val Predefined = List(Url, Email, Slug, DateUS, DateEurope, Time12H, Time24H, PhoneNumUS, ZipCodeUS)

    def of(raw: String): Regexp = Predefined.find(_.underlying.toString() === raw).getOrElse(Regexp.Custom(raw.r))

final case class ContentType(
    id: ContentTypeId,
    name: String,
    displayField: Option[String],
    description: Option[String],
    fields: List[Field],
    version: Option[Int]
):
  def isCompatible(other: ContentType): Boolean =
    id === other.id &&
      name === other.name &&
      displayField === other.displayField &&
      other.fields.filter(_.required).toSet === fields.filter(_.required).toSet &&
      other.fields.filterNot(_.required).toSet.subsetOf(fields.filterNot(_.required).toSet)

object ContentType:

  given eq: Eq[ContentType] = (a, b) =>
    a.id === b.id &&
      a.name === b.name &&
      a.description === b.description &&
      a.displayField === b.displayField &&
      a.fields === b.fields
