package io.dantb.contentless.webhook

sealed trait WebhookFilter

object WebhookFilter:
  final case class Equals(property: PropertyPath, operand: String)          extends WebhookFilter
  final case class Includes(property: PropertyPath, operands: List[String]) extends WebhookFilter
  final case class Regex(property: PropertyPath, pattern: String)           extends WebhookFilter
  final case class Not(filter: WebhookFilter)                               extends WebhookFilter
