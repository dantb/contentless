package io.dantb.contentless

import cats.data.NonEmptyList
import org.scalacheck.{Arbitrary, Gen}

object arbitrary:

  // newtypes
  given Arbitrary[ContentTypeId] = Arbitrary(genUuidString.map(ContentTypeId(_)))

  given Arbitrary[MimeTypeGroup] = Arbitrary(genMimeTypeGroup)

  given [A: Arbitrary]: Arbitrary[NonEmptyList[A]] = Arbitrary(genNel(summon[Arbitrary[A]].arbitrary))

  def genNel[A](genA: Gen[A]): Gen[NonEmptyList[A]] = Gen.nonEmptyListOf(genA).map(NonEmptyList.fromListUnsafe(_))

  def genUuidString: Gen[String] = Gen.uuid.map(_.toString())

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
