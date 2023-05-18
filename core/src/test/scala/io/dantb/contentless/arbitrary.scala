package io.dantb.contentless

import cats.data.NonEmptyList
import io.dantb.contentless.RichText.Mark
import io.dantb.contentless.RichTextNodeType
import io.dantb.contentless.Validation.{LinkContentType, RichTextNodes}
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Arbitrary.{arbitrary as arb}

object arbitrary:

  // library types
  given [A: Arbitrary]: Arbitrary[NonEmptyList[A]] = Arbitrary(genNel(summon[Arbitrary[A]].arbitrary))

  // newtypes
  given Arbitrary[ContentTypeId] = Arbitrary(genUuidString.map(ContentTypeId(_)))

  // domain-specific types
  given Arbitrary[MimeTypeGroup]                = Arbitrary(genMimeTypeGroup)
  given Arbitrary[RichTextNodeType]             = Arbitrary(genRichTextNodeType)
  given Arbitrary[Mark]                         = Arbitrary(genMark)
  given Arbitrary[LinkContentType]              = Arbitrary(genLinkContentType)
  given Arbitrary[Validation.Size]              = Arbitrary(genSize)
  given Arbitrary[RichTextNodes.EntryHyperlink] = Arbitrary(genEntryHyperlink)
  given Arbitrary[RichTextNodes.EntryBlock]     = Arbitrary(genEntryBlock)
  given Arbitrary[RichTextNodes.EntryInline]    = Arbitrary(genEntryInline)

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

  def genRichTextNodeType: Gen[RichTextNodeType] = Gen.oneOf(RichTextNodeType.All)

  def genMark: Gen[Mark] = Gen.oneOf(Mark.All)

  def genEntryHyperlink: Gen[RichTextNodes.EntryHyperlink] = for
    size            <- opt[Validation.Size]
    linkContentType <- opt[LinkContentType]
  yield RichTextNodes.EntryHyperlink(size, linkContentType)

  def genEntryBlock: Gen[RichTextNodes.EntryBlock] = for
    size            <- opt[Validation.Size]
    linkContentType <- opt[LinkContentType]
  yield RichTextNodes.EntryBlock(size, linkContentType)

  def genEntryInline: Gen[RichTextNodes.EntryInline] = for
    size            <- opt[Validation.Size]
    linkContentType <- opt[LinkContentType]
  yield RichTextNodes.EntryInline(size, linkContentType)

  def genSize: Gen[Validation.Size] = for
    min     <- opt[Int]
    max     <- opt[Int]
    message <- opt[String]
  yield Validation.Size(min, max, message)

  def genLinkContentType: Gen[LinkContentType] = for
    allowedContentTypes <- arb[Set[String]]
    message             <- opt[String]
  yield LinkContentType(allowedContentTypes, message)

  // utilities
  def opt[A: Arbitrary]: Gen[Option[A]] = Gen.option(arb[A])
