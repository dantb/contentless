package io.dantb.contentless

import cats.Eq
import cats.syntax.all.*

sealed trait FieldType

object FieldType {

  implicit val eq: Eq[FieldType] = Eq.instance {
    case (a: Text, b: Text)         => a.longText === b.longText && a.validations === b.validations
    case (a: RichText, b: RichText) => a.validations === b.validations
    case (a: Array, b: Array)       =>
      // using the `val eq` explicitly here, since implicitly using
      // the val while inside the closure is now considered dangerous and incurs a compile-error under -Wself-implicit
      eq.eqv(a.itemType, b.itemType) && a.minLength === b.minLength && a.maxLength === b.maxLength
    case (a, b) => a == b
  }

  final case class Text(longText: Boolean, validations: Set[Validation])                      extends FieldType
  final case class Media(mimeTypeGroup: Set[MimeTypeGroup])                                   extends FieldType
  final case class Reference(linkContentTypes: Set[ContentTypeId])                            extends FieldType
  final case class Array(itemType: FieldType, minLength: Option[Int], maxLength: Option[Int]) extends FieldType
  final case class RichText(validations: Set[Validation])                                     extends FieldType
  case object Integer                                                                         extends FieldType
  case object Number                                                                          extends FieldType
  case object Boolean                                                                         extends FieldType
  case object Json                                                                            extends FieldType
  case object DateTime                                                                        extends FieldType
  case object Location                                                                        extends FieldType
}
