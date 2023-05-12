package io.dantb.contentless

import cats.Show

sealed trait ContentfulError extends Product with Serializable

object ContentfulError {
  final case class Conflict(message: String)                      extends ContentfulError
  final case class DecodeError(reason: String)                    extends ContentfulError
  final case class FileAlreadyProcessed(message: String)          extends ContentfulError
  final case class MissingVersion(message: String)                extends ContentfulError
  final case class UnexpectedResponse(code: Int, message: String) extends ContentfulError
  final case class AssetPublishFailed(message: String)            extends ContentfulError
  final case class PublishFailed(message: String)                 extends ContentfulError

  implicit val showInstance: Show[ContentfulError] = {
    case Conflict(s)                 => s"ContentfulError (Conflict): $s"
    case DecodeError(s)              => s"ContentfulError (DecodeError): $s"
    case FileAlreadyProcessed(s)     => s"ContentfulError (FileAlreadyProcessed): $s"
    case MissingVersion(s)           => s"ContentfulError (MissingVersion): $s"
    case UnexpectedResponse(code, s) => s"ContentfulError (UnexpectedResponse): $code: $s"
    case AssetPublishFailed(s)       => s"ContentfulError (AssetPublishFailed): $s"
    case PublishFailed(s)            => s"ContentfulError (PublishFailed): $s"
  }
}
