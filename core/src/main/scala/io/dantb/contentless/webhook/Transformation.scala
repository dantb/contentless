package io.dantb.contentless.webhook

import io.circe.Json

/** For ways in which Contentful webhooks can be customised, see:
  * https://www.contentful.com/developers/docs/references/content-management-api/#transforming-webhook-calls
  */
final case class Transformation(
    method: Option[Method],
    contentType: Option[ContentTypeHeader],
    includeContentLength: Option[Boolean],
    body: Option[Json]
)

object Transformation {
  def empty: Transformation = Transformation(None, None, None, None)
}
