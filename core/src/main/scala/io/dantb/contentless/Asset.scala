package io.dantb.contentless

import cats.syntax.all.*
import io.dantb.contentless.circe.implicits.given
import io.dantb.contentless.circe.implicits.*

final case class Asset(title: String, file: File)

object Asset:
  given assetEntryCodec: EntryCodec[Asset] =
    (text("title", "Title").required, json[File]("file", "File").required)
      .imapN(Asset.apply)(Function.unlift(asset => Some((asset.title, asset.file))))

final case class File(contentType: String, fileName: String, upload: String)
