package io.dantb.contentless

final case class Locale(code: String) extends AnyVal

object Locale:
  val enUS: Locale = Locale("en-US")
  val enGB: Locale = Locale("en-GB")
