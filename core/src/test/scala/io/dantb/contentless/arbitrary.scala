package io.dantb.contentless

import org.scalacheck.{Arbitrary, Gen}

object arbitrary:

  given Arbitrary[MimeTypeGroup] = Arbitrary(genMimeTypeGroup)

  def genMimeTypeGroup: Gen[MimeTypeGroup] = Gen.oneOf(
    MimeTypeGroup.Archive,
    MimeTypeGroup.Attachment,
    MimeTypeGroup.Audio,
    MimeTypeGroup.Code,
    MimeTypeGroup.Image,
    MimeTypeGroup.Markup,
    MimeTypeGroup.Pdfdocument,
    MimeTypeGroup.Plaintext,
    MimeTypeGroup.Presentation,
    MimeTypeGroup.Richtext,
    MimeTypeGroup.Spreadsheet,
    MimeTypeGroup.Video
  )
