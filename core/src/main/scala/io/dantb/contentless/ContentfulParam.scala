package io.dantb.contentless

sealed trait ContentfulParam:
  def key: String
  def value: String

object ContentfulParam:
  def apply(k: String, v: String): ContentfulParam =
    new ContentfulParam:
      def key: String   = k
      def value: String = v

  final case class Filter(key: String, value: String) extends ContentfulParam
  final case class Include(level: Int) extends ContentfulParam:
    val key: String   = "include"
    val value: String = level.toString
  final case class Locale(value: String) extends ContentfulParam:
    val key: String = "locale"
  final case class ContentType(value: String) extends ContentfulParam:
    val key: String = "content_type"
  final case class Limit(n: Int) extends ContentfulParam:
    val key: String   = "limit"
    val value: String = n.toString
