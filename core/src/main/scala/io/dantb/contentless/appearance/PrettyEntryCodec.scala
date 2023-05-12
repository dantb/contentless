package io.dantb.contentless.appearance

import java.time.{LocalDateTime, ZonedDateTime}

import cats.InvariantMonoidal
import cats.syntax.all.*
import io.circe.{Decoder, Encoder}
import io.dantb.contentless._
import io.dantb.contentless.FieldCodec.Dsl
import io.dantb.contentless.RichText.Node
import io.dantb.contentless.appearance.Control.*
import io.dantb.contentless.appearance.Editor.BuiltIn.EntryEditor

sealed abstract case class PrettyEntryCodec[A](
    entryCodec: EntryCodec[A],
    controls: Set[FieldControl],
    extension: Option[Editor.Extension] = None,
    app: Option[Editor.App] = None,
    sidebarWidgets: Option[List[SidebarWidget]] = None
):
  self =>
  val editorInterface: EditorInterface = EditorInterface(
    extension // Use extension or app if present, disabling default editor.
      .map[Set[Editor]](e => Set(e, EntryEditor(disabled = true)))
      .orElse[Set[Editor]](app.map(a => Set(a, EntryEditor(disabled = true))))
      .getOrElse(Editor.Default), // Fall through to default editor.
    sidebarWidgets.getOrElse(SidebarWidget.Defaults),
    controls
  )

  def imap[B](f: A => B)(g: B => A): PrettyEntryCodec[B] =
    eimap(f(_).asRight)(g)

  def eimap[B](f: A => Either[String, B])(g: B => A): PrettyEntryCodec[B] =
    new PrettyEntryCodec(entryCodec.eimap(f)(g), controls, extension, app, sidebarWidgets) {}

  def product[B](other: PrettyEntryCodec[B]): PrettyEntryCodec[(A, B)] =
    new PrettyEntryCodec(
      entryCodec.product(other.entryCodec),
      controls ++ other.controls,
      extension.orElse(other.extension),
      app.orElse(other.app),
      sidebarWidgets.orElse(other.sidebarWidgets)
    ) {}

  def withEditorExtension(e: Editor.Extension): PrettyEntryCodec[A] =
    new PrettyEntryCodec(entryCodec, controls, Some(e), None, sidebarWidgets) {}

  def withEditorApp(a: Editor.App): PrettyEntryCodec[A] =
    new PrettyEntryCodec(entryCodec, controls, None, Some(a), sidebarWidgets) {}

  def withSidebar(widgets: List[SidebarWidget]): PrettyEntryCodec[A] =
    new PrettyEntryCodec(entryCodec, controls, extension, app, Some(widgets)) {}

object PrettyEntryCodec:

  def apply[A](using codec: PrettyEntryCodec[A]): PrettyEntryCodec[A] = codec

  def unit: PrettyEntryCodec[Unit] = new PrettyEntryCodec[Unit](EntryCodec.unit, Set.empty) {}

  implicit def invariantMonoidal: InvariantMonoidal[PrettyEntryCodec] =
    new InvariantMonoidal[PrettyEntryCodec]:
      override def unit: PrettyEntryCodec[Unit] = PrettyEntryCodec.unit
      override def product[A, B](
          fa: PrettyEntryCodec[A],
          fb: PrettyEntryCodec[B]
      ): PrettyEntryCodec[(A, B)] =
        fa.product(fb)
      override def imap[A, B](fa: PrettyEntryCodec[A])(f: A => B)(g: B => A): PrettyEntryCodec[B] =
        fa.imap(f)(g)

sealed trait PrettyFieldCodec[A]:
  def required: PrettyEntryCodec[A] =
    new PrettyEntryCodec(fieldCodec.required, Set(FieldControl(fieldCodec.fieldId, control, settings))) {}

  def optional: PrettyEntryCodec[Option[A]] =
    new PrettyEntryCodec(fieldCodec.optional, Set(FieldControl(fieldCodec.fieldId, control, settings))) {}

  protected def control: Control
  protected def fieldCodec: FieldCodec[A]
  protected def settings: Set[FieldControlSetting]

private[contentless] object PrettyFieldCodec:
  trait PrettyDsl extends Dsl:
    def prettyLongText(
        fieldId: String,
        fieldName: String,
        validations: Set[Validation] = Set.empty,
        defaultValue: Option[String] = None
    ): PrettyLongTextField =
      new PrettyLongTextField(super.text(fieldId, fieldName, validations, longText = true, defaultValue)) {}

    def prettyText(
        fieldId: String,
        fieldName: String,
        validations: Set[Validation] = Set.empty,
        defaultValue: Option[String] = None
    ): PrettyTextField =
      new PrettyTextField(super.text(fieldId, fieldName, validations, defaultValue = defaultValue)) {}

    def prettyBoolean(fieldId: String, fieldName: String, defaultValue: Option[Boolean] = None): PrettyBoolField =
      new PrettyBoolField(super.boolean(fieldId, fieldName, defaultValue)) {}

    def prettyInt(fieldId: String, fieldName: String, defaultValue: Option[Int] = None): PrettyIntField =
      new PrettyIntField(super.integer(fieldId, fieldName, defaultValue)) {}

    def prettyDecimal(fieldId: String, fieldName: String, defaultValue: Option[Double] = None): PrettyNumField =
      new PrettyNumField(super.decimal(fieldId, fieldName, defaultValue)) {}

    def prettyJson[A: Encoder: Decoder](fieldId: String, fieldName: String): PrettyJsonField[A] =
      new PrettyJsonField[A](super.json[A](fieldId, fieldName)) {}

    def prettyDateTime(
        fieldId: String,
        fieldName: String,
        defaultValue: Option[LocalDateTime] = None
    ): PrettyDateTimeField[LocalDateTime] =
      new PrettyDateTimeField[LocalDateTime](super.dateTime(fieldId, fieldName, defaultValue)) {}

    def prettyZonedDateTime(
        fieldId: String,
        fieldName: String,
        defaultValue: Option[ZonedDateTime] = None
    ): PrettyDateTimeField[ZonedDateTime] =
      new PrettyDateTimeField[ZonedDateTime](super.zonedDateTime(fieldId, fieldName, defaultValue)) {}.zoned

    def prettyLocation(fieldId: String, fieldName: String): PrettyLocationField =
      new PrettyLocationField(super.location(fieldId, fieldName)) {}

    def prettyRichText(fieldId: String, fieldName: String, validations: Set[Validation] = Set.empty): PrettyRichTextField =
      new PrettyRichTextField(super.richText(fieldId, fieldName, validations)) {}

    // Contentful don't support text lists for "Text", only "Symbol" (short text): https://www.contentful.com/developers/docs/concepts/data-model/#array-fields
    def prettyTextList(
        fieldId: String,
        fieldName: String,
        validations: Set[Validation] = Set.empty,
        minLength: Option[Int] = None,
        maxLength: Option[Int] = None,
        defaultValue: Option[List[String]] = None
    ): PrettyTextListField =
      new PrettyTextListField(
        super.textList(fieldId, fieldName, validations, minLength, maxLength, longText = false, defaultValue)
      ) {}

    def prettyMedia(fieldId: String, fieldName: String, mimeTypeGroup: Set[MimeTypeGroup]): PrettyAssetField =
      new PrettyAssetField(super.media(fieldId, fieldName, mimeTypeGroup)) {}

    def prettyReference(fieldId: String, fieldName: String, linkContentTypes: Set[ContentTypeId]): PrettyEntryField =
      new PrettyEntryField(super.reference(fieldId, fieldName, linkContentTypes)) {}

    def prettyReferences(
        fieldId: String,
        fieldName: String,
        linkContentTypes: Set[ContentTypeId],
        minLength: Option[Int] = None,
        maxLength: Option[Int] = None
    ): PrettyEntriesField =
      new PrettyEntriesField(super.references(fieldId, fieldName, linkContentTypes, minLength, maxLength)) {}

    /** Lift an abitrary entry codec into a pretty context. Useful for when you want to combine a shared entry codec into a
      * pretty entry codec.
      */
    implicit final class PrettyOps[A](codec: EntryCodec[A]):
      def withBuiltInAppearances = new PrettyEntryCodec[A](codec, Set.empty, None, None) {}

sealed abstract case class PrettyBoolField(
    fieldCodec: FieldCodec[Boolean],
    boolControl: BoolControl = Control.BuiltIn.Boolean.boolean
) extends PrettyFieldCodec[Boolean]:

  override val control: Control = boolControl.value

  private def copy(
      fieldCodec: FieldCodec[Boolean] = this.fieldCodec,
      boolControl: BoolControl = this.boolControl
  ): PrettyBoolField =
    new PrettyBoolField(fieldCodec, boolControl) {}

  def withControl(c: BoolControl): PrettyBoolField = this.copy(boolControl = c)
  def disabled: PrettyBoolField                    = this.copy(fieldCodec = fieldCodec.disabled)
  def settings: Set[FieldControlSetting] =
    boolControl.trueLabel.toSet ++ boolControl.falseLabel.toSet ++ boolControl.settings ++ boolControl.helpText.toSet

sealed abstract case class PrettyLongTextField(
    fieldCodec: FieldCodec[String],
    textControl: LongTextControl = Control.BuiltIn.Markdown.longText
) extends PrettyFieldCodec[String]:

  override val control: Control = textControl.value

  private def copy(
      fieldCodec: FieldCodec[String] = this.fieldCodec,
      textControl: LongTextControl = this.textControl
  ): PrettyLongTextField =
    new PrettyLongTextField(fieldCodec, textControl) {}

  def withControl(c: LongTextControl): PrettyLongTextField = this.copy(textControl = c)
  def disabled: PrettyLongTextField                        = this.copy(fieldCodec = fieldCodec.disabled)
  def settings: Set[FieldControlSetting]                   = textControl.settings ++ textControl.helpText.toSet

sealed abstract case class PrettyTextField(
    fieldCodec: FieldCodec[String],
    textControl: TextControl = Control.BuiltIn.SingleLine.text
) extends PrettyFieldCodec[String]:

  override val control: Control = textControl.value

  private def copy(
      fieldCodec: FieldCodec[String] = this.fieldCodec,
      textControl: TextControl = this.textControl
  ): PrettyTextField =
    new PrettyTextField(fieldCodec, textControl) {}

  def withControl(c: TextControl): PrettyTextField = this.copy(textControl = c)
  def disabled: PrettyTextField                    = this.copy(fieldCodec = fieldCodec.disabled)
  def settings: Set[FieldControlSetting]           = textControl.settings ++ textControl.helpText.toSet

sealed abstract case class PrettyIntField(
    fieldCodec: FieldCodec[Int],
    intControl: IntControl = Control.BuiltIn.NumberEditor.integer
) extends PrettyFieldCodec[Int]:

  override val control: Control = intControl.value

  private def copy(
      fieldCodec: FieldCodec[Int] = this.fieldCodec,
      intControl: IntControl = this.intControl
  ): PrettyIntField =
    new PrettyIntField(fieldCodec, intControl) {}

  def withControl(c: IntControl): PrettyIntField = this.copy(intControl = c)
  def disabled: PrettyIntField                   = this.copy(fieldCodec = fieldCodec.disabled)
  def settings: Set[FieldControlSetting] =
    intControl.stars.toSet ++ intControl.settings ++ intControl.helpText.toSet

sealed abstract case class PrettyNumField(
    fieldCodec: FieldCodec[Double],
    numControl: NumControl = Control.BuiltIn.NumberEditor.number
) extends PrettyFieldCodec[Double]:

  override val control: Control = numControl.value

  private def copy(
      fieldCodec: FieldCodec[Double] = this.fieldCodec,
      numControl: NumControl = this.numControl
  ): PrettyNumField =
    new PrettyNumField(fieldCodec, numControl) {}

  def withControl(c: NumControl): PrettyNumField = this.copy(numControl = c)
  def disabled: PrettyNumField                   = this.copy(fieldCodec = fieldCodec.disabled)
  def settings: Set[FieldControlSetting] =
    numControl.stars.toSet[FieldControlSetting] ++ numControl.settings ++ numControl.helpText.toSet

sealed abstract case class PrettyJsonField[A](
    fieldCodec: FieldCodec[A],
    jsonControl: JsonControl = Control.BuiltIn.ObjectEditor.json
) extends PrettyFieldCodec[A]:

  override val control: Control = jsonControl.value

  private def copy(
      fieldCodec: FieldCodec[A] = this.fieldCodec,
      jsonControl: JsonControl = this.jsonControl
  ): PrettyJsonField[A] =
    new PrettyJsonField(fieldCodec, jsonControl) {}

  def withControl(c: JsonControl): PrettyJsonField[A] = this.copy(jsonControl = c)
  def disabled: PrettyJsonField[A]                    = this.copy(fieldCodec = fieldCodec.disabled)
  def settings: Set[FieldControlSetting]              = jsonControl.settings ++ jsonControl.helpText.toSet

sealed abstract case class PrettyDateTimeField[A](
    fieldCodec: FieldCodec[A],
    dateTimeControl: DateTimeControl = Control.BuiltIn.DatePicker.dateTime
) extends PrettyFieldCodec[A]:

  override val control: Control = dateTimeControl.value

  private def copy(
      fieldCodec: FieldCodec[A] = this.fieldCodec,
      dateTimeControl: DateTimeControl = this.dateTimeControl
  ): PrettyDateTimeField[A] =
    new PrettyDateTimeField(fieldCodec, dateTimeControl) {}

  def withControl(c: DateTimeControl): PrettyDateTimeField[A] = this.copy(dateTimeControl = c)
  def disabled: PrettyDateTimeField[A]                        = this.copy(fieldCodec = fieldCodec.disabled)
  def settings: Set[FieldControlSetting] =
    dateTimeControl.format.toSet ++ dateTimeControl.clockType.toSet ++ dateTimeControl.settings ++ dateTimeControl.helpText.toSet

  def zoned = copy(dateTimeControl = dateTimeControl.withFormat(FieldControlSetting.DatePicker.Format.TimeZ))

sealed abstract case class PrettyLocationField(
    fieldCodec: FieldCodec[Location],
    locControl: LocationControl = Control.BuiltIn.LocationEditor.location
) extends PrettyFieldCodec[Location]:

  override val control: Control = locControl.value

  private def copy(
      fieldCodec: FieldCodec[Location] = this.fieldCodec,
      locControl: LocationControl = this.locControl
  ): PrettyLocationField =
    new PrettyLocationField(fieldCodec, locControl) {}

  def withControl(c: LocationControl): PrettyLocationField = this.copy(locControl = c)
  def disabled: PrettyLocationField                        = this.copy(fieldCodec = fieldCodec.disabled)
  def settings: Set[FieldControlSetting]                   = locControl.settings ++ locControl.helpText.toSet

sealed abstract case class PrettyRichTextField(
    fieldCodec: FieldCodec[Node],
    richTextControl: RichTextControl = Control.BuiltIn.RichTextEditor.richText
) extends PrettyFieldCodec[Node]:

  override val control: Control = richTextControl.value

  private def copy(
      fieldCodec: FieldCodec[Node] = this.fieldCodec,
      richTextControl: RichTextControl = this.richTextControl
  ): PrettyRichTextField =
    new PrettyRichTextField(fieldCodec, richTextControl) {}

  def withControl(c: RichTextControl): PrettyRichTextField = this.copy(richTextControl = c)
  def disabled: PrettyRichTextField                        = this.copy(fieldCodec = fieldCodec.disabled)
  def settings: Set[FieldControlSetting]                   = richTextControl.settings ++ richTextControl.helpText.toSet

sealed abstract case class PrettyTextListField(
    fieldCodec: FieldCodec[List[String]],
    textList: TextListControl = Control.BuiltIn.TagEditor.textList
) extends PrettyFieldCodec[List[String]]:

  override val control: Control = textList.value

  private def copy(
      fieldCodec: FieldCodec[List[String]] = this.fieldCodec,
      textList: TextListControl = this.textList
  ): PrettyTextListField =
    new PrettyTextListField(fieldCodec, textList) {}

  def withControl(c: TextListControl): PrettyTextListField = this.copy(textList = c)
  def disabled: PrettyTextListField                        = this.copy(fieldCodec = fieldCodec.disabled)
  def settings: Set[FieldControlSetting]                   = textList.settings ++ textList.helpText.toSet

sealed abstract case class PrettyAssetField(
    fieldCodec: FieldCodec[Media],
    assetControl: AssetControl = Control.BuiltIn.AssetLinkEditor.asset
) extends PrettyFieldCodec[Media]:

  override val control: Control = assetControl.value

  private def copy(
      fieldCodec: FieldCodec[Media] = this.fieldCodec,
      assetControl: AssetControl = this.assetControl
  ): PrettyAssetField = new PrettyAssetField(fieldCodec, assetControl) {}

  def withControl(c: AssetControl): PrettyAssetField = this.copy(assetControl = c)
  def disabled: PrettyAssetField                     = this.copy(fieldCodec = fieldCodec.disabled)
  def settings: Set[FieldControlSetting] =
    assetControl.showCreateEntity.toSet ++ assetControl.showLinkEntity.toSet ++ assetControl.settings ++ assetControl.helpText.toSet

sealed abstract case class PrettyEntryField(
    fieldCodec: FieldCodec[Reference],
    entryControl: EntryControl = Control.BuiltIn.EntryLinkEditor.entry
) extends PrettyFieldCodec[Reference]:

  override val control: Control = entryControl.value

  private def copy(
      fieldCodec: FieldCodec[Reference] = this.fieldCodec,
      entryControl: EntryControl = this.entryControl
  ): PrettyEntryField = new PrettyEntryField(fieldCodec, entryControl) {}

  def withControl(c: EntryControl): PrettyEntryField = this.copy(entryControl = c)
  def disabled: PrettyEntryField                     = this.copy(fieldCodec = fieldCodec.disabled)
  def settings: Set[FieldControlSetting] =
    entryControl.showCreateEntity.toSet ++ entryControl.showLinkEntity.toSet ++ entryControl.settings ++ entryControl.helpText.toList

sealed abstract case class PrettyEntriesField(
    fieldCodec: FieldCodec[List[Reference]],
    entriesControl: EntriesControl = Control.BuiltIn.EntryLinksEditor.entries
) extends PrettyFieldCodec[List[Reference]]:

  override val control: Control = entriesControl.value

  private def copy(
      fieldCodec: FieldCodec[List[Reference]] = this.fieldCodec,
      entryControl: EntriesControl = this.entriesControl
  ): PrettyEntriesField = new PrettyEntriesField(fieldCodec, entryControl) {}

  def withControl(c: EntriesControl): PrettyEntriesField = this.copy(entryControl = c)
  def disabled: PrettyEntriesField                       = this.copy(fieldCodec = fieldCodec.disabled)
  def settings: Set[FieldControlSetting] =
    entriesControl.showCreateEntity.toSet ++ entriesControl.showLinkEntity.toSet ++ entriesControl.bulkEditing.toSet ++ entriesControl.settings ++ entriesControl.helpText.toSet
