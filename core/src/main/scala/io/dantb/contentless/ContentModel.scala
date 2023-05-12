package io.dantb.contentless

import cats.{Eq, Show}
import cats.syntax.all.*
import io.circe.Json

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
trait ContentModel[A] {

  /** The primary identifier in Contentful for this Content Model */
  def contentType: ContentTypeId

  /** The Human-Friendly name for this Content Model */
  def contentTypeName: String

  /** The field from the Model that will be used for displaying instances of this Content Model within contentful */
  def displayField: Option[String]

  /** The Human-Friendly description with the raison d'être of for instances of this Content Model */
  def description: Option[String]
}

object ContentModel {
  def apply[A](using contentModel: ContentModel[A]): ContentModel[A] = contentModel
}

/** The ID of the content type. Must be unique (within each environment) */
final case class ContentTypeId(asString: String) extends AnyVal

object ContentTypeId {
  given eq: Eq[ContentTypeId]     = Eq.by(_.asString)
  given show: Show[ContentTypeId] = _.asString

  def of[A: ContentModel]: ContentTypeId =
    ContentModel[A].contentType
}

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

object Field {
  given eq: Eq[Field] = Eq.instance { (a, b) =>
    a.id === b.id && a.name === b.name && a.required === b.required && a.disabled === b.disabled &&
    a.fieldType === b.fieldType && a.defaultValue === b.defaultValue
  }
}

sealed trait Validation extends Product with Serializable

object Validation {
  given eq: Eq[Validation] = Eq.fromUniversalEquals

  final case class ContainedIn(allowedValues: List[String]) extends Validation

  final case class RichTextNodes(
      assetHyperlinkSize: Option[Size],
      entryHyperlink: Option[RichTextNodes.EntryHyperlink],
      assetBlockSize: Option[Size],
      entryBlock: Option[RichTextNodes.EntryBlock],
      entryInline: Option[RichTextNodes.EntryInline]
  ) extends Validation
  object RichTextNodes {
    def empty: RichTextNodes = RichTextNodes(None, None, None, None, None)

    final case class EntryInline(size: Option[Size], linkContentType: Option[LinkContentType])
    final case class EntryHyperlink(size: Option[Size], linkContentType: Option[LinkContentType])
    final case class EntryBlock(size: Option[Size], linkContentType: Option[LinkContentType])
  }

  final case class LinkContentType(allowedContentTypes: Set[String], message: Option[String]) extends Validation
  final case class RichTextMarks(allowedValues: Set[String])                                  extends Validation
  final case class RichTextNodeTypes(allowedValues: Set[String])                              extends Validation
  final case class Size(min: Option[Int], max: Option[Int], message: Option[String])          extends Validation
  case object Unique                                                                          extends Validation

  // equality defers to underlying regex, to allow custom regex subtypes for convenience without breaking equality
  sealed abstract case class Regexp(regexp: String) extends Validation {
    override def equals(obj: Any): Boolean = obj.isInstanceOf[Regexp] && obj.asInstanceOf[Regexp].regexp == regexp
    override def hashCode(): Int           = regexp.hashCode
  }
  object ValidUrl extends Regexp(Regexp.validUrlRegexp)
  object Regexp {
    val validUrlRegexp = "^(ftp|http|https):\\/\\/(\\w+:{0,1}\\w*@)?(\\S+)(:[0-9]+)?(\\/|\\/([\\w#!:.?+=&%@!\\-\\/]))?$"

    def apply(str: String): Regexp = new Regexp(str) {}
  }
}

final case class ContentType(
    id: ContentTypeId,
    name: String,
    displayField: Option[String],
    description: Option[String],
    fields: List[Field],
    version: Option[Int]
) {
  def isCompatible(other: ContentType): Boolean =
    id === other.id &&
      name === other.name &&
      displayField === other.displayField &&
      other.fields.filter(_.required).toSet === fields.filter(_.required).toSet &&
      other.fields.filterNot(_.required).toSet.subsetOf(fields.filterNot(_.required).toSet)
}

object ContentType {

  given eq: Eq[ContentType] = (a, b) =>
    a.id === b.id &&
      a.name === b.name &&
      a.description === b.description &&
      a.displayField === b.displayField &&
      a.fields === b.fields

}
