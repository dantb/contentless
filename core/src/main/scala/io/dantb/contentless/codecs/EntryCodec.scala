package io.dantb.contentless.codecs

import java.time.{LocalDateTime, ZonedDateTime}

import cats.InvariantMonoidal
import cats.syntax.all.*
import io.circe.{Decoder, Encoder, Json}
import io.circe.syntax.*
import io.dantb.contentless.*
import io.dantb.contentless.RichText.Node
import io.dantb.contentless.appearance.*
import io.dantb.contentless.appearance.Control.*
import io.dantb.contentless.appearance.Editor.BuiltIn.EntryEditor
import io.dantb.contentless.codecs.EntryCodec
import io.dantb.contentless.codecs.implicits.given
import io.dantb.contentless.dsl.*

sealed abstract case class FieldCodec[A](
    fieldId: String,
    fieldType: FieldType,
    fieldName: String,
    defaultValue: Option[A],
    locale: Locale = defaultLocale,
    isDisabled: Boolean = false
)(using encoder: Encoder[A], decoder: Decoder[A]):
  self =>
  def required: EntryCodec[A] =
    new EntryCodec[A](Set(FieldControl(fieldId, control, settings))):
      override def read(fields: Map[String, Json]): Either[String, A] =
        fields
          .get(fieldId)
          .toRight(s"Field not found: $fieldId ($fieldType)")
          .flatMap(_.as[Map[String, Json]].leftMap(_.toString()))
          .flatMap(_.get(locale.code).toRight(s"Localization not found for ${locale.code}"))
          .flatMap(_.as[A].leftMap(_.toString()))
          .leftMap(err => s"$err occurred when parsing $fieldId of type $fieldType")
      override def write(value: A): Map[String, Json] =
        val localized = Map(locale.code -> value.asJson).asJson
        Map(fieldId -> localized)
      override def schema: List[Field] =
        List(
          Field(
            fieldId,
            fieldName,
            required = true,
            isDisabled,
            fieldType,
            defaultValue.map(value => Map(locale.code -> value.asJson)).getOrElse(Map.empty)
          )
        )

  def optional: EntryCodec[Option[A]] =
    new EntryCodec[Option[A]](Set(FieldControl(fieldId, control, settings))):
      override def read(fields: Map[String, Json]): Either[String, Option[A]] =
        fields
          .get(fieldId)
          .traverse[Either[String, *], A] {
            _.as[Map[String, Json]]
              .leftMap(_.toString())
              .flatMap(_.get(locale.code).toRight(s"Localization not found for ${locale.code}"))
              .flatMap(_.as[A].leftMap(_.toString()))
          }
          .leftMap(err => s"$err occurred when parsing $fieldId of type $fieldType")

      override def write(option: Option[A]): Map[String, Json] =
        option match
          case Some(value) =>
            val localized = Map(locale.code -> value.asJson).asJson
            Map(fieldId -> localized)
          case None => Map.empty

      override def schema: List[Field] =
        List(
          Field(
            fieldId,
            fieldName,
            required = false,
            isDisabled,
            fieldType,
            defaultValue.map(value => Map(locale.code -> value.asJson)).getOrElse(Map.empty)
          )
        )

  def withLocale(newLocale: Locale): FieldCodec[A] = new FieldCodec(
    fieldId,
    fieldType,
    fieldName,
    defaultValue,
    newLocale,
    isDisabled
  ):
    def control: Control                   = self.control
    def settings: Set[FieldControlSetting] = self.settings

  def disabled: FieldCodec[A] =
    new FieldCodec(
      fieldId,
      fieldType,
      fieldName,
      defaultValue,
      locale,
      isDisabled = true
    ):
      def control: Control                   = self.control
      def settings: Set[FieldControlSetting] = self.settings

  protected def control: Control
  protected def settings: Set[FieldControlSetting]

sealed abstract case class EntryCodec[A](
    controls: Set[FieldControl],
    extension: Option[Editor.Extension] = None,
    app: Option[Editor.App] = None,
    sidebarWidgets: Option[List[SidebarWidget]] = None
):
  self =>

  def schema: List[Field]
  def read(input: Map[String, Json]): Either[String, A]
  def write(value: A): Map[String, Json]

  val editorInterface: EditorInterface = EditorInterface(
    extension // Use extension or app if present, disabling default editor.
      .map[Set[Editor]](e => Set(e, EntryEditor(disabled = true)))
      .orElse[Set[Editor]](app.map(a => Set(a, EntryEditor(disabled = true))))
      .getOrElse(Editor.Default), // Fall through to default editor.
    sidebarWidgets.getOrElse(SidebarWidget.Defaults),
    controls
  )

  def imap[B](f: A => B)(g: B => A): EntryCodec[B] =
    eimap(f(_).asRight)(g)

  def eimap[B](f: A => Either[String, B])(g: B => A): EntryCodec[B] =
    new EntryCodec[B](controls, extension, app, sidebarWidgets):
      override def read(input: Map[String, Json]): Either[String, B] = self.read(input).flatMap(f)
      override def write(value: B): Map[String, Json]                = self.write(g(value))
      override val schema: List[Field]                               = self.schema

  def product[B](other: EntryCodec[B]): EntryCodec[(A, B)] =
    new EntryCodec[(A, B)](
      controls ++ other.controls,
      extension.orElse(other.extension),
      app.orElse(other.app),
      sidebarWidgets.orElse(other.sidebarWidgets)
    ):
      override def read(input: Map[String, Json]): Either[String, (A, B)] = self.read(input).product(other.read(input))
      override def write(value: (A, B)): Map[String, Json]                = self.write(value._1) ++ other.write(value._2)
      override val schema: List[Field]                                    = self.schema ++ other.schema

  def withEditorExtension(e: Editor.Extension): EntryCodec[A] =
    new EntryCodec[A](controls, Some(e), None, sidebarWidgets):
      override def read(input: Map[String, Json]): Either[String, A] = self.read(input)
      override def write(value: A): Map[String, Json]                = self.write(value)
      override val schema: List[Field]                               = self.schema

  def withEditorApp(a: Editor.App): EntryCodec[A] =
    new EntryCodec[A](controls, None, Some(a), sidebarWidgets):
      override def read(input: Map[String, Json]): Either[String, A] = self.read(input)
      override def write(value: A): Map[String, Json]                = self.write(value)
      override val schema: List[Field]                               = self.schema

  def withSidebar(widgets: List[SidebarWidget]): EntryCodec[A] =
    new EntryCodec[A](controls, extension, app, Some(widgets)):
      override def read(input: Map[String, Json]): Either[String, A] = self.read(input)
      override def write(value: A): Map[String, Json]                = self.write(value)
      override val schema: List[Field]                               = self.schema

object EntryCodec:

  def apply[A](using codec: EntryCodec[A]): EntryCodec[A] = codec

  def unit: EntryCodec[Unit] = new EntryCodec[Unit](Set.empty):
    override def read(input: Map[String, Json]): Either[String, Unit] = Right(())
    override def write(value: Unit): Map[String, Json]                = Map.empty
    override val schema: List[Field]                                  = Nil

  implicit def invariantMonoidal: InvariantMonoidal[EntryCodec] =
    new InvariantMonoidal[EntryCodec]:
      override def unit: EntryCodec[Unit] = EntryCodec.unit
      override def product[A, B](fa: EntryCodec[A], fb: EntryCodec[B]): EntryCodec[(A, B)] =
        fa.product(fb)
      override def imap[A, B](fa: EntryCodec[A])(f: A => B)(g: B => A): EntryCodec[B] = fa.imap(f)(g)

object FieldCodec:
  val defaultLocale: Locale = Locale.enGB

  // def apply[A: Encoder: Decoder](
  //     fieldId: String,
  //     fieldType: FieldType,
  //     fieldName: String,
  //     defaultValue: Option[A]
  // ): FieldCodec[A] = FieldCodec(fieldId, fieldType, fieldName, defaultLocale, isDisabled = false, defaultValue)

  trait Dsl:

    def longText(
        fieldId: String,
        fieldName: String,
        validations: Set[Validation] = Set.empty,
        defaultValue: Option[String] = None
    ): LongTextField =
      new LongTextField(fieldId, fieldName, validations, defaultValue) {}

    def text(
        fieldId: String,
        fieldName: String,
        validations: Set[Validation] = Set.empty,
        defaultValue: Option[String] = None
    ): TextField =
      new TextField(fieldId, fieldName, validations, defaultValue) {}

    def boolean(fieldId: String, fieldName: String, defaultValue: Option[Boolean] = None): BoolField =
      new BoolField(fieldId, fieldName, defaultValue) {}

    def int(fieldId: String, fieldName: String, defaultValue: Option[Int] = None): IntField =
      new IntField(fieldId, fieldName, defaultValue) {}

    def decimal(fieldId: String, fieldName: String, defaultValue: Option[Double] = None): NumField =
      new NumField(fieldId, fieldName, defaultValue) {}

    def json[A: Encoder: Decoder](fieldId: String, fieldName: String): JsonField[A] =
      new JsonField[A](fieldId, fieldName) {}

    def dateTime(
        fieldId: String,
        fieldName: String,
        defaultValue: Option[LocalDateTime] = None
    ): DateTimeField[LocalDateTime] =
      new DateTimeField[LocalDateTime](fieldId, fieldName, defaultValue) {}

    def zonedDateTime(
        fieldId: String,
        fieldName: String,
        defaultValue: Option[ZonedDateTime] = None
    ): DateTimeField[ZonedDateTime] =
      new DateTimeField[ZonedDateTime](fieldId, fieldName, defaultValue) {}.zoned

    def location(fieldId: String, fieldName: String): LocationField =
      new LocationField(fieldId, fieldName) {}

    def richText(fieldId: String, fieldName: String, validations: Set[Validation] = Set.empty): RichTextField =
      new RichTextField(fieldId, fieldName) {}

    // Contentful don't support text lists for "Text", only "Symbol" (short text): https://www.contentful.com/developers/docs/concepts/data-model/#array-fields
    def textList(
        fieldId: String,
        fieldName: String,
        validations: Set[Validation] = Set.empty,
        minLength: Option[Int] = None,
        maxLength: Option[Int] = None,
        defaultValue: Option[List[String]] = None
    ): TextListField =
      new TextListField(
        fieldId,
        fieldName,
        validations,
        minLength,
        maxLength,
        defaultValue
      ) {}

    def media(fieldId: String, fieldName: String, mimeTypeGroup: Set[MimeTypeGroup]): AssetField =
      new AssetField(fieldId, fieldName, mimeTypeGroup) {}

    def reference(fieldId: String, fieldName: String, linkContentTypes: Set[ContentTypeId]): EntryField =
      new EntryField(fieldId, fieldName, linkContentTypes) {}

    def references(
        fieldId: String,
        fieldName: String,
        linkContentTypes: Set[ContentTypeId],
        minLength: Option[Int] = None,
        maxLength: Option[Int] = None
    ): EntriesField =
      new EntriesField(
        fieldId,
        fieldName,
        linkContentTypes,
        minLength,
        maxLength
      ) {}

sealed abstract class BoolField(
    fieldId: String,
    fieldName: String,
    defaultValue: Option[Boolean] = None,
    boolControl: BoolControl = Control.BuiltIn.Boolean.boolean
) extends FieldCodec[Boolean](fieldId, FieldType.Boolean, fieldName, defaultValue):

  override val control: Control = boolControl.value

  def withControl(c: BoolControl): BoolField =
    new BoolField(fieldId, fieldName, defaultValue, c) {}
  def settings: Set[FieldControlSetting] =
    boolControl.trueLabel.toSet ++ boolControl.falseLabel.toSet ++ boolControl.settings ++ boolControl.helpText.toSet

sealed abstract class LongTextField(
    fieldId: String,
    fieldName: String,
    validations: Set[Validation] = Set.empty,
    defaultValue: Option[String] = None,
    textControl: LongTextControl = Control.BuiltIn.Markdown.longText
) extends FieldCodec[String](fieldId, FieldType.Text(longText = true, validations), fieldName, defaultValue):

  override val control: Control = textControl.value

  def withControl(c: LongTextControl): LongTextField =
    new LongTextField(fieldId, fieldName, validations, defaultValue, c) {}

  def settings: Set[FieldControlSetting] = textControl.settings ++ textControl.helpText.toSet

sealed abstract class TextField(
    fieldId: String,
    fieldName: String,
    validations: Set[Validation] = Set.empty,
    defaultValue: Option[String] = None,
    textControl: TextControl = Control.BuiltIn.SingleLine.text
) extends FieldCodec[String](fieldId, FieldType.Text(longText = false, validations), fieldName, defaultValue):

  override val control: Control = textControl.value

  def withControl(c: TextControl): TextField =
    new TextField(fieldId, fieldName, validations, defaultValue, c) {}
  def settings: Set[FieldControlSetting] = textControl.settings ++ textControl.helpText.toSet

sealed abstract class IntField(
    fieldId: String,
    fieldName: String,
    defaultValue: Option[Int] = None,
    intControl: IntControl = Control.BuiltIn.NumberEditor.integer
) extends FieldCodec[Int](fieldId, FieldType.Integer, fieldName, defaultValue):

  override val control: Control = intControl.value

  def withControl(c: IntControl): IntField =
    new IntField(fieldId, fieldName, defaultValue, c) {}
  def settings: Set[FieldControlSetting] =
    intControl.stars.toSet ++ intControl.settings ++ intControl.helpText.toSet

sealed abstract class NumField(
    fieldId: String,
    fieldName: String,
    defaultValue: Option[Double] = None,
    numControl: NumControl = Control.BuiltIn.NumberEditor.number
) extends FieldCodec[Double](fieldId, FieldType.Number, fieldName, defaultValue):

  override val control: Control = numControl.value

  def withControl(c: NumControl): NumField =
    new NumField(fieldId, fieldName, defaultValue, c) {}
  def settings: Set[FieldControlSetting] =
    numControl.stars.toSet[FieldControlSetting] ++ numControl.settings ++ numControl.helpText.toSet

sealed abstract class JsonField[A: Encoder: Decoder](
    fieldId: String,
    fieldName: String,
    jsonControl: JsonControl = Control.BuiltIn.ObjectEditor.json
) extends FieldCodec[A](fieldId, FieldType.Json, fieldName, None):

  override val control: Control = jsonControl.value

  def withControl(c: JsonControl): JsonField[A] =
    new JsonField(fieldId, fieldName, c) {}
  def settings: Set[FieldControlSetting] = jsonControl.settings ++ jsonControl.helpText.toSet

sealed abstract class DateTimeField[A: Encoder: Decoder](
    fieldId: String,
    fieldName: String,
    defaultValue: Option[A] = None,
    dateTimeControl: DateTimeControl = Control.BuiltIn.DatePicker.dateTime
) extends FieldCodec[A](fieldId, FieldType.DateTime, fieldName, defaultValue):

  override val control: Control = dateTimeControl.value

  def withControl(c: DateTimeControl): DateTimeField[A] =
    new DateTimeField(fieldId, fieldName, defaultValue, c) {}
  def settings: Set[FieldControlSetting] =
    dateTimeControl.format.toSet ++ dateTimeControl.clockType.toSet ++ dateTimeControl.settings ++ dateTimeControl.helpText.toSet

  def zoned = new DateTimeField(
    fieldId,
    fieldName,
    defaultValue,
    dateTimeControl.withFormat(FieldControlSetting.DatePicker.Format.TimeZ)
  ) {}

sealed abstract class LocationField(
    fieldId: String,
    fieldName: String,
    locControl: LocationControl = Control.BuiltIn.LocationEditor.location
) extends FieldCodec[Location](fieldId, FieldType.Location, fieldName, None):

  override val control: Control = locControl.value

  def withControl(c: LocationControl): LocationField =
    new LocationField(fieldId, fieldName, c) {}
  def settings: Set[FieldControlSetting] = locControl.settings ++ locControl.helpText.toSet

sealed abstract class RichTextField(
    fieldId: String,
    fieldName: String,
    validations: Set[Validation] = Set.empty,
    richTextControl: RichTextControl = Control.BuiltIn.RichTextEditor.richText
) extends FieldCodec[Node](fieldId, FieldType.RichText(validations), fieldName, None):

  override val control: Control = richTextControl.value

  def withControl(c: RichTextControl): RichTextField =
    new RichTextField(fieldId, fieldName, validations, c) {}
  def settings: Set[FieldControlSetting] = richTextControl.settings ++ richTextControl.helpText.toSet

sealed abstract class TextListField(
    fieldId: String,
    fieldName: String,
    validations: Set[Validation] = Set.empty,
    minLength: Option[Int] = None,
    maxLength: Option[Int] = None,
    defaultValue: Option[List[String]] = None,
    textList: TextListControl = Control.BuiltIn.TagEditor.textList
) extends FieldCodec[List[String]](
      fieldId,
      FieldType.Array(FieldType.Text(longText = false, validations), minLength, maxLength),
      fieldName,
      defaultValue
    ):

  override val control: Control = textList.value

  def withControl(c: TextListControl): TextListField =
    new TextListField(fieldId, fieldName, validations, minLength, maxLength, defaultValue, c) {}
  def settings: Set[FieldControlSetting] = textList.settings ++ textList.helpText.toSet

sealed abstract class AssetField(
    fieldId: String,
    fieldName: String,
    mimeTypeGroup: Set[MimeTypeGroup],
    assetControl: AssetControl = Control.BuiltIn.AssetLinkEditor.asset
) extends FieldCodec[Media](fieldId, FieldType.Media(mimeTypeGroup), fieldName, None):

  override val control: Control = assetControl.value

  def withControl(c: AssetControl): AssetField =
    new AssetField(fieldId, fieldName, mimeTypeGroup, c) {}
  def settings: Set[FieldControlSetting] =
    assetControl.showCreateEntity.toSet ++ assetControl.showLinkEntity.toSet ++ assetControl.settings ++ assetControl.helpText.toSet

sealed abstract class EntryField(
    fieldId: String,
    fieldName: String,
    linkContentTypes: Set[ContentTypeId],
    entryControl: EntryControl = Control.BuiltIn.EntryLinkEditor.entry
) extends FieldCodec[Reference](fieldId, FieldType.Reference(linkContentTypes), fieldName, None):

  override val control: Control = entryControl.value

  def withControl(c: EntryControl): EntryField =
    new EntryField(fieldId, fieldName, linkContentTypes, c) {}
  def settings: Set[FieldControlSetting] =
    entryControl.showCreateEntity.toSet ++ entryControl.showLinkEntity.toSet ++ entryControl.settings ++ entryControl.helpText.toList

sealed abstract class EntriesField(
    fieldId: String,
    fieldName: String,
    linkContentTypes: Set[ContentTypeId],
    minLength: Option[Int] = None,
    maxLength: Option[Int] = None,
    entriesControl: EntriesControl = Control.BuiltIn.EntryLinksEditor.entries
) extends FieldCodec[List[Reference]](
      fieldId,
      FieldType.Array(FieldType.Reference(linkContentTypes), minLength, maxLength),
      fieldName,
      None
    ):

  override val control: Control = entriesControl.value

  def withControl(c: EntriesControl): EntriesField =
    new EntriesField(fieldId, fieldName, linkContentTypes, minLength, maxLength, c) {}
  def settings: Set[FieldControlSetting] =
    entriesControl.showCreateEntity.toSet ++ entriesControl.showLinkEntity.toSet ++ entriesControl.bulkEditing.toSet ++ entriesControl.settings ++ entriesControl.helpText.toSet
