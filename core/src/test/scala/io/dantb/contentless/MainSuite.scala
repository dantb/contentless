/*
 * Copyright 2023 Typelevel
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package io.dantb.contentless

import java.time.{LocalDateTime, ZonedDateTime}

import cats.syntax.all.*
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import io.dantb.contentless.RichText.{Node, Paragraph}
import io.dantb.contentless.appearance.{Control, Editor, EditorInterface, FieldControl, PrettyEntryCodec, SidebarWidget}
import io.dantb.contentless.appearance.FieldControlSetting.{CustomSetting, HelpText}
import io.dantb.contentless.appearance.FieldControlSetting.Boolean.{FalseLabel, TrueLabel}
import io.dantb.contentless.appearance.FieldControlSetting.DatePicker.{ClockType, Format}
import io.dantb.contentless.appearance.FieldControlSetting.LinksEditor.{
  BulkEditing,
  ShowCreateEntityAction,
  ShowLinkEntityAction
}
import io.dantb.contentless.appearance.SidebarWidget.BuiltIn.{InfoPanel, Publication}
import io.dantb.contentless.circe.implicits.{prettyMedia, *}

final case class PrettyPerson(
    name: String,
    bio: String,
    age: Int,
    family: List[Reference],
    birthday: ZonedDateTime,
    birthdayAlt: LocalDateTime,
    location: Option[Location],
    height: Double,
    likesEggs: Boolean,
    profilePic: Option[Media],
    body: Node,
    bestFriend: Option[Reference]
)

object PrettyPerson {
  val NameControl: Control.TextControl = Control.BuiltIn.UrlEditor.text.withHelpText("Enter your name")
  val FamilyControl: Control.EntriesControl =
    Control.BuiltIn.EntryCardsEditor.entries.withShowLinkEntity(false).withHelpText("Family tree")
  val LocationControl: Control.LocationControl = Control
    .Extension("custom-loc-control")
    .location(CustomSetting("custom-setting", Json.obj("cs-field1" -> 1.asJson, "cs-field2" -> true.asJson)))
  val EggsControl: Control.BoolControl =
    Control.BuiltIn.Boolean.boolean.withTrueLabel("They love eggs").withFalseLabel("They hate cholesterol")

  val PersonEditorApp: Editor.App              = Editor.App("this-is-a-custom-editor")
  val PersonCustomSidebarWidget: SidebarWidget = SidebarWidget.Extension("this-is-a-custom-sidebar-widget")
  val PersonSidebar: List[SidebarWidget]       = List(Publication, PersonCustomSidebarWidget, InfoPanel)

  given codec: PrettyEntryCodec[PrettyPerson] =
    (
      prettyText("name", "Name", Set(Validation.ContainedIn(List("John", "Jack")))).withControl(NameControl).required,
      prettyLongText(
        "bio",
        "Biography",
        Set(Validation.Size(Some(50), Some(300), Some("Max 300 chars"))),
        defaultValue = "Not very interesting bio".some
      ).required,
      prettyInt("age", "Age").disabled.required,
      prettyReferences("family", "Family", Set(ContentTypeId("person"))).withControl(FamilyControl).required,
      prettyZonedDateTime("birthday", "Birthday").required,
      prettyDateTime("birthdayAlt", "Birthday Alt").required,
      prettyLocation("location", "Location").withControl(LocationControl).optional,
      prettyDecimal("height", "Height").required,
      prettyBoolean("likesEggs", "Likes eggs").withControl(EggsControl).disabled.required,
      prettyMedia("profilePic", "Profile picture", Set(MimeTypeGroup.Image)).optional,
      prettyRichText("body", "Body", Set(Validation.RichTextNodeTypes(Set("paragraph")))).required,
      prettyReference("bestFriend", "BFF", Set(ContentTypeId("person"))).optional
    ).imapN(PrettyPerson.apply)(x =>
      (
        x.name,
        x.bio,
        x.age,
        x.family,
        x.birthday,
        x.birthdayAlt,
        x.location,
        x.height,
        x.likesEggs,
        x.profilePic,
        x.body,
        x.bestFriend
      )
    ).withEditorApp(PersonEditorApp)
      .withSidebar(PersonSidebar)
}

final case class PrettyAllMimeTypeGroupMedia(
    archiveMedia: Media,
    attachmentMedia: Media,
    audioMedia: Media,
    codeMedia: Media,
    imageMedia: Media,
    markupMedia: Media,
    pdfdocumentMedia: Media,
    plaintextMedia: Media,
    presentationMedia: Media,
    richtextMedia: Media,
    spreadsheetMedia: Media,
    videoMedia: Media,
    multiMedia: Media
)
object PrettyAllMimeTypeGroupMedia {
  given codec: PrettyEntryCodec[PrettyAllMimeTypeGroupMedia] = (
    prettyMedia("archiveMedia", "Archive Media", Set(MimeTypeGroup.Archive)).required,
    prettyMedia("attachmentMedia", "Attachment Media", Set(MimeTypeGroup.Attachment)).required,
    prettyMedia("audioMedia", "Audio Media", Set(MimeTypeGroup.Audio)).required,
    prettyMedia("codeMedia", "Code Media", Set(MimeTypeGroup.Code)).required,
    prettyMedia("imageMedia", "Image Media", Set(MimeTypeGroup.Image)).required,
    prettyMedia("markupMedia", "Markup Media", Set(MimeTypeGroup.Markup)).required,
    prettyMedia("pdfdocumentMedia", "Pdfdocument Media", Set(MimeTypeGroup.Pdfdocument)).required,
    prettyMedia("plaintextMedia", "Plaintext Media", Set(MimeTypeGroup.Plaintext)).required,
    prettyMedia("presentationMedia", "Presentation Media", Set(MimeTypeGroup.Presentation)).required,
    prettyMedia("richtextMedia", "Richtext Media", Set(MimeTypeGroup.Richtext)).required,
    prettyMedia("spreadsheetMedia", "Spreadsheet Media", Set(MimeTypeGroup.Spreadsheet)).required,
    prettyMedia("videoMedia", "Video Media", Set(MimeTypeGroup.Video)).required,
    prettyMedia(
      "multiMedia",
      "Multi Media",
      Set(MimeTypeGroup.Video, MimeTypeGroup.Audio, MimeTypeGroup.Richtext, MimeTypeGroup.Image)
    ).required
  ).imapN(PrettyAllMimeTypeGroupMedia.apply)(x =>
    (
      x.archiveMedia,
      x.attachmentMedia,
      x.audioMedia,
      x.codeMedia,
      x.imageMedia,
      x.markupMedia,
      x.pdfdocumentMedia,
      x.plaintextMedia,
      x.presentationMedia,
      x.richtextMedia,
      x.spreadsheetMedia,
      x.videoMedia,
      x.multiMedia
    )
  )
}

class PrettyEntryCodecSpec extends munit.FunSuite {
  test("correctly encode/decode an entry with editor interfaces") {
    val expectedSchema: List[Field] =
      List(
        Field(
          "name",
          "Name",
          required = true,
          disabled = false,
          FieldType.Text(longText = false, Set(Validation.ContainedIn(List("John", "Jack")))),
          Map.empty
        ),
        Field(
          "bio",
          "Biography",
          required = true,
          disabled = false,
          FieldType.Text(longText = true, Set(Validation.Size(Some(50), Some(300), Some("Max 300 chars")))),
          Map("en-GB" -> "Not very interesting bio".asJson)
        ),
        Field("age", "Age", required = true, disabled = true, FieldType.Integer, Map.empty),
        Field(
          "family",
          "Family",
          required = true,
          disabled = false,
          FieldType.Array(FieldType.Reference(Set(ContentTypeId("person"))), None, None),
          Map.empty
        ),
        Field("birthday", "Birthday", required = true, disabled = false, FieldType.DateTime, Map.empty),
        Field("birthdayAlt", "Birthday Alt", required = true, disabled = false, FieldType.DateTime, Map.empty),
        Field("location", "Location", required = false, disabled = false, FieldType.Location, Map.empty),
        Field("height", "Height", required = true, disabled = false, FieldType.Number, Map.empty),
        Field("likesEggs", "Likes eggs", required = true, disabled = true, FieldType.Boolean, Map.empty),
        Field(
          "profilePic",
          "Profile picture",
          required = false,
          disabled = false,
          FieldType.Media(Set(MimeTypeGroup.Image)),
          Map.empty
        ),
        Field(
          "body",
          "Body",
          required = true,
          disabled = false,
          FieldType.RichText(Set(Validation.RichTextNodeTypes(Set("paragraph")))),
          Map.empty
        ),
        Field(
          "bestFriend",
          "BFF",
          required = false,
          disabled = false,
          FieldType.Reference(Set(ContentTypeId("person"))),
          Map.empty
        )
      )

    val bday    = ZonedDateTime.parse("2019-11-20T11:50:54.417Z")
    val bdayAlt = LocalDateTime.parse("2019-11-20T11:50:54.417")
    val person = PrettyPerson(
      "John",
      "Loves a bagel",
      56,
      List(Reference("ad")),
      bday,
      bdayAlt,
      Location(1.234, 5.678).some,
      205,
      likesEggs = true,
      None,
      Paragraph(Nil),
      None
    )
    val expectedJson = Map(
      "name"        -> Json.obj("en-GB" -> person.name.asJson),
      "bio"         -> Json.obj("en-GB" -> person.bio.asJson),
      "age"         -> Json.obj("en-GB" -> person.age.asJson),
      "family"      -> Json.obj("en-GB" -> person.family.asJson),
      "birthday"    -> Json.obj("en-GB" -> person.birthday.asJson),
      "birthdayAlt" -> Json.obj("en-GB" -> person.birthdayAlt.asJson),
      "location"    -> Json.obj("en-GB" -> JsonSamples.locationJson),
      "height"      -> Json.obj("en-GB" -> person.height.asJson),
      "likesEggs"   -> Json.obj("en-GB" -> person.likesEggs.asJson),
      "body"        -> Json.obj("en-GB" -> person.body.asJson)
    )

    val expectedEditorInterface = EditorInterface(
      Set(Editor.App("this-is-a-custom-editor"), Editor.BuiltIn.EntryEditor(disabled = true)),
      PrettyPerson.PersonSidebar,
      Set(
        FieldControl("name", PrettyPerson.NameControl.value, Set(HelpText("Enter your name"))),
        FieldControl("bio", Control.BuiltIn.Markdown, Set.empty),
        FieldControl("age", Control.BuiltIn.NumberEditor, Set.empty),
        FieldControl(
          "family",
          PrettyPerson.FamilyControl.value,
          Set(ShowCreateEntityAction(true), ShowLinkEntityAction(false), BulkEditing(false), HelpText("Family tree"))
        ),
        FieldControl("birthday", Control.BuiltIn.DatePicker, Set(Format.TimeZ, ClockType.TwentyFourHour)),
        FieldControl("birthdayAlt", Control.BuiltIn.DatePicker, Set(Format.Time, ClockType.TwentyFourHour)),
        FieldControl(
          "location",
          PrettyPerson.LocationControl.value,
          Set(CustomSetting("custom-setting", Json.obj("cs-field1" -> 1.asJson, "cs-field2" -> true.asJson)))
        ),
        FieldControl("height", Control.BuiltIn.NumberEditor, Set.empty),
        FieldControl(
          "likesEggs",
          PrettyPerson.EggsControl.value,
          Set(TrueLabel("They love eggs"), FalseLabel("They hate cholesterol"))
        ),
        FieldControl(
          "profilePic",
          Control.BuiltIn.AssetLinkEditor,
          Set(ShowCreateEntityAction(true), ShowLinkEntityAction(true))
        ),
        FieldControl("body", Control.BuiltIn.RichTextEditor, Set.empty),
        FieldControl(
          "bestFriend",
          Control.BuiltIn.EntryLinkEditor,
          Set(ShowCreateEntityAction(true), ShowLinkEntityAction(true))
        )
      )
    )

    assertEquals(
      PrettyEntryCodec[PrettyPerson].entryCodec.schema,
      expectedSchema
    )
    assertEquals(
      PrettyEntryCodec[PrettyPerson].entryCodec.write(person),
      expectedJson
    )
    assertEquals(
      PrettyEntryCodec[PrettyPerson].entryCodec.read(expectedJson),
      Right(person)
    )
    assertEquals(
      PrettyEntryCodec[PrettyPerson].editorInterface,
      expectedEditorInterface
    )

  }

  test("correctly encode/decode an entry with all mime type groups") {
    val expectedSchema: List[Field] = List(
      Field("archiveMedia", "Archive Media", true, false, FieldType.Media(Set(MimeTypeGroup.Archive)), Map.empty),
      Field("attachmentMedia", "Attachment Media", true, false, FieldType.Media(Set(MimeTypeGroup.Attachment)), Map.empty),
      Field("audioMedia", "Audio Media", true, false, FieldType.Media(Set(MimeTypeGroup.Audio)), Map.empty),
      Field("codeMedia", "Code Media", true, false, FieldType.Media(Set(MimeTypeGroup.Code)), Map.empty),
      Field("imageMedia", "Image Media", true, false, FieldType.Media(Set(MimeTypeGroup.Image)), Map.empty),
      Field("markupMedia", "Markup Media", true, false, FieldType.Media(Set(MimeTypeGroup.Markup)), Map.empty),
      Field(
        "pdfdocumentMedia",
        "Pdfdocument Media",
        true,
        false,
        FieldType.Media(Set(MimeTypeGroup.Pdfdocument)),
        Map.empty
      ),
      Field("plaintextMedia", "Plaintext Media", true, false, FieldType.Media(Set(MimeTypeGroup.Plaintext)), Map.empty),
      Field(
        "presentationMedia",
        "Presentation Media",
        true,
        false,
        FieldType.Media(Set(MimeTypeGroup.Presentation)),
        Map.empty
      ),
      Field("richtextMedia", "Richtext Media", true, false, FieldType.Media(Set(MimeTypeGroup.Richtext)), Map.empty),
      Field(
        "spreadsheetMedia",
        "Spreadsheet Media",
        true,
        false,
        FieldType.Media(Set(MimeTypeGroup.Spreadsheet)),
        Map.empty
      ),
      Field("videoMedia", "Video Media", true, false, FieldType.Media(Set(MimeTypeGroup.Video)), Map.empty),
      Field(
        "multiMedia",
        "Multi Media",
        true,
        false,
        FieldType.Media(Set(MimeTypeGroup.Video, MimeTypeGroup.Audio, MimeTypeGroup.Richtext, MimeTypeGroup.Image)),
        Map.empty
      )
    )

    val entity = PrettyAllMimeTypeGroupMedia(
      archiveMedia = Media("1"),
      attachmentMedia = Media("2"),
      audioMedia = Media("3"),
      codeMedia = Media("4"),
      imageMedia = Media("5"),
      markupMedia = Media("6"),
      pdfdocumentMedia = Media("7"),
      plaintextMedia = Media("8"),
      presentationMedia = Media("9"),
      richtextMedia = Media("10"),
      spreadsheetMedia = Media("11"),
      videoMedia = Media("12"),
      multiMedia = Media("13")
    )

    def assetJson(id: String) = Json.obj(
      "en-GB" -> Json.obj("sys" -> Json.obj("type" -> "Link".asJson, "linkType" -> "Asset".asJson, "id" -> id.asJson))
    )

    val expectedJson = Map(
      "archiveMedia"      -> assetJson("1"),
      "attachmentMedia"   -> assetJson("2"),
      "audioMedia"        -> assetJson("3"),
      "codeMedia"         -> assetJson("4"),
      "imageMedia"        -> assetJson("5"),
      "markupMedia"       -> assetJson("6"),
      "pdfdocumentMedia"  -> assetJson("7"),
      "plaintextMedia"    -> assetJson("8"),
      "presentationMedia" -> assetJson("9"),
      "richtextMedia"     -> assetJson("10"),
      "spreadsheetMedia"  -> assetJson("11"),
      "videoMedia"        -> assetJson("12"),
      "multiMedia"        -> assetJson("13")
    )

    assertEquals(
      PrettyEntryCodec[PrettyAllMimeTypeGroupMedia].entryCodec.schema,
      expectedSchema
    )
    assertEquals(
      PrettyEntryCodec[PrettyAllMimeTypeGroupMedia].entryCodec.write(entity),
      expectedJson
    )
    assertEquals(
      PrettyEntryCodec[PrettyAllMimeTypeGroupMedia].entryCodec.read(expectedJson),
      Right(entity)
    )
  }

  test("lift ordinary entry codecs into a pretty context") {
    val codec = EntryCodec.unit
    assertEquals(codec.withBuiltInAppearances.entryCodec, codec)
  }
}
