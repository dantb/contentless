package io.dantb.contentless.webhook

import cats.Eq
import io.dantb.contentless.Versioned

final case class WebhookDefinition(
    id: String,
    name: String,
    url: String,
    topics: List[WebhookTopic],
    filters: List[WebhookFilter],
    headers: List[WebhookHeader],
    version: Option[Int],
    transformation: Transformation
)

object WebhookDefinition:
  given eq: Eq[WebhookDefinition] = Versioned.eqUnversionedUniversalEquals

final case class WebhookHeader(key: String, value: Option[String], secret: Boolean):
  override def toString: String =
    if secret then s"{ $key = *** }"
    else s"{ $key = $value }"

  override def equals(obj: Any): Boolean =
    obj match
      case WebhookHeader(otherKey, _, true)           => key == otherKey
      case WebhookHeader(otherKey, otherValue, false) => key == otherKey && value == otherValue
      case _                                          => false

final case class WebhookTopic(event: WebhookEvent, entityType: EntityType)
