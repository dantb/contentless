package io.dantb.contentless

enum MimeTypeGroup(val typeName: String):
  case Archive      extends MimeTypeGroup("archive")
  case Attachment   extends MimeTypeGroup("attachment")
  case Audio        extends MimeTypeGroup("audio")
  case Code         extends MimeTypeGroup("code")
  case Image        extends MimeTypeGroup("image")
  case Markup       extends MimeTypeGroup("markup")
  case Pdfdocument  extends MimeTypeGroup("pdfdocument")
  case Plaintext    extends MimeTypeGroup("plaintext")
  case Presentation extends MimeTypeGroup("presentation")
  case Richtext     extends MimeTypeGroup("richtext")
  case Spreadsheet  extends MimeTypeGroup("spreadsheet")
  case Video        extends MimeTypeGroup("video")
