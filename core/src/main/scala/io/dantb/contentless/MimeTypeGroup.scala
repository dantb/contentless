package io.dantb.contentless

sealed abstract class MimeTypeGroup(val typeName: String)

object MimeTypeGroup:
  case object Archive      extends MimeTypeGroup("archive")
  case object Attachment   extends MimeTypeGroup("attachment")
  case object Audio        extends MimeTypeGroup("audio")
  case object Code         extends MimeTypeGroup("code")
  case object Image        extends MimeTypeGroup("image")
  case object Markup       extends MimeTypeGroup("markup")
  case object Pdfdocument  extends MimeTypeGroup("pdfdocument")
  case object Plaintext    extends MimeTypeGroup("plaintext")
  case object Presentation extends MimeTypeGroup("presentation")
  case object Richtext     extends MimeTypeGroup("richtext")
  case object Spreadsheet  extends MimeTypeGroup("spreadsheet")
  case object Video        extends MimeTypeGroup("video")
