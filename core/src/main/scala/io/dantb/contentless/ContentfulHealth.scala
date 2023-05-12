package io.dantb.contentless

sealed trait ContentfulHealth extends Product with Serializable

object ContentfulHealth:
  case object Ok                          extends ContentfulHealth
  final case class Failing(error: String) extends ContentfulHealth
