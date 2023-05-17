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
import org.typelevel.twiddles.TwiddleSyntax

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

object EntryCodec extends TwiddleSyntax[EntryCodec]:

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

sealed abstract case class FieldCodec[A](
    fieldId: String,
    fieldType: FieldType,
    fieldName: String,
    defaultValue: Option[A],
    control: Control,
    settings: Set[FieldControlSetting],
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
    control,
    settings,
    newLocale,
    isDisabled
  ) {}

  def disabled: FieldCodec[A] =
    new FieldCodec(
      fieldId,
      fieldType,
      fieldName,
      defaultValue,
      control,
      settings,
      locale,
      isDisabled = true
    ) {}

object FieldCodec:
  val defaultLocale: Locale = Locale.enGB

  // TODO only allow validations in DSL in line with the docs: https://www.contentful.com/help/available-validations/
  trait Dsl:

    def longText(
        fieldId: String,
        fieldName: String,
        validations: Set[Validation] = Set.empty,
        defaultValue: Option[String] = None,
        textControl: LongTextControl = Control.BuiltIn.Markdown.longText
    ): FieldCodec[String] =
      new FieldCodec[String](
        fieldId,
        FieldType.Text(longText = true, validations),
        fieldName,
        defaultValue,
        textControl.value,
        textControl.settings ++ textControl.helpText.toSet
      ) {}

    def text(
        fieldId: String,
        fieldName: String,
        validations: Set[Validation] = Set.empty,
        defaultValue: Option[String] = None,
        textControl: TextControl = Control.BuiltIn.SingleLine.text
    ): FieldCodec[String] =
      new FieldCodec[String](
        fieldId,
        FieldType.Text(longText = false, validations),
        fieldName,
        defaultValue,
        textControl.value,
        textControl.settings ++ textControl.helpText.toSet
      ) {}

    def boolean(
        fieldId: String,
        fieldName: String,
        defaultValue: Option[Boolean] = None,
        boolControl: BoolControl = Control.BuiltIn.Boolean.boolean
    ): FieldCodec[Boolean] =
      new FieldCodec[Boolean](
        fieldId,
        FieldType.Boolean,
        fieldName,
        defaultValue,
        boolControl.value,
        boolControl.trueLabel.toSet ++ boolControl.falseLabel.toSet ++ boolControl.settings ++ boolControl.helpText.toSet
      ) {}

    def int(
        fieldId: String,
        fieldName: String,
        defaultValue: Option[Int] = None,
        intControl: IntControl = Control.BuiltIn.NumberEditor.integer
    ): FieldCodec[Int] =
      new FieldCodec[Int](
        fieldId,
        FieldType.Integer,
        fieldName,
        defaultValue,
        intControl.value,
        intControl.stars.toSet ++ intControl.settings ++ intControl.helpText.toSet
      ) {}

    def decimal(
        fieldId: String,
        fieldName: String,
        defaultValue: Option[Double] = None,
        numControl: NumControl = Control.BuiltIn.NumberEditor.number
    ): FieldCodec[Double] =
      new FieldCodec[Double](
        fieldId,
        FieldType.Number,
        fieldName,
        defaultValue,
        numControl.value,
        numControl.stars.toSet[FieldControlSetting] ++ numControl.settings ++ numControl.helpText.toSet
      ) {}

    def json[A: Encoder: Decoder](
        fieldId: String,
        fieldName: String,
        jsonControl: JsonControl = Control.BuiltIn.ObjectEditor.json
    ): FieldCodec[A] =
      new FieldCodec[A](
        fieldId,
        FieldType.Json,
        fieldName,
        None,
        jsonControl.value,
        jsonControl.settings ++ jsonControl.helpText.toSet
      ) {}

    def dateTime(
        fieldId: String,
        fieldName: String,
        defaultValue: Option[LocalDateTime] = None,
        dateTimeControl: DateTimeControl = Control.BuiltIn.DatePicker.dateTime
    ): FieldCodec[LocalDateTime] =
      new FieldCodec[LocalDateTime](
        fieldId,
        FieldType.DateTime,
        fieldName,
        defaultValue,
        dateTimeControl.value,
        dateTimeControl.format.toSet ++ dateTimeControl.clockType.toSet ++ dateTimeControl.settings ++ dateTimeControl.helpText.toSet
      ) {}

    def zonedDateTime(
        fieldId: String,
        fieldName: String,
        defaultValue: Option[ZonedDateTime] = None,
        dateTimeControl: DateTimeControl = Control.BuiltIn.DatePicker.dateTime
    ): FieldCodec[ZonedDateTime] =
      new FieldCodec[ZonedDateTime](
        fieldId,
        FieldType.DateTime,
        fieldName,
        defaultValue,
        dateTimeControl.zoned.value,
        dateTimeControl.format.toSet ++ dateTimeControl.clockType.toSet ++ dateTimeControl.settings ++ dateTimeControl.helpText.toSet
      ) {}

    def location(
        fieldId: String,
        fieldName: String,
        locControl: LocationControl = Control.BuiltIn.LocationEditor.location
    ): FieldCodec[Location] =
      new FieldCodec[Location](
        fieldId,
        FieldType.Location,
        fieldName,
        None,
        locControl.value,
        locControl.settings ++ locControl.helpText.toSet
      ) {}

    def richText(
        fieldId: String,
        fieldName: String,
        validations: Set[Validation] = Set.empty,
        richTextControl: RichTextControl = Control.BuiltIn.RichTextEditor.richText
    ): FieldCodec[Node] =
      new FieldCodec[Node](
        fieldId,
        FieldType.RichText(validations),
        fieldName,
        None,
        richTextControl.value,
        richTextControl.settings ++ richTextControl.helpText.toSet
      ) {}

    // Contentful don't support text lists for "Text", only "Symbol" (short text): https://www.contentful.com/developers/docs/concepts/data-model/#array-fields
    def textList(
        fieldId: String,
        fieldName: String,
        validations: Set[Validation] = Set.empty,
        minLength: Option[Int] = None,
        maxLength: Option[Int] = None,
        defaultValue: Option[List[String]] = None,
        textList: TextListControl = Control.BuiltIn.TagEditor.textList
    ): FieldCodec[List[String]] =
      new FieldCodec[List[String]](
        fieldId,
        FieldType.Array(FieldType.Text(longText = false, validations), minLength, maxLength),
        fieldName,
        defaultValue,
        textList.value,
        textList.settings ++ textList.helpText.toSet
      ) {}

    def asset(
        fieldId: String,
        fieldName: String,
        mimeTypeGroup: Set[MimeTypeGroup],
        assetControl: AssetControl = Control.BuiltIn.AssetLinkEditor.asset
    ): FieldCodec[Media] =
      new FieldCodec[Media](
        fieldId,
        FieldType.Media(mimeTypeGroup),
        fieldName,
        None,
        assetControl.value,
        assetControl.showCreateEntity.toSet ++ assetControl.showLinkEntity.toSet ++ assetControl.settings ++ assetControl.helpText.toSet
      ) {}

    def assets(
        fieldId: String,
        fieldName: String,
        mimeTypeGroup: Set[MimeTypeGroup],
        minLength: Option[Int] = None,
        maxLength: Option[Int] = None,
        assetsControl: AssetsControl = Control.BuiltIn.AssetLinksEditor.assets
    ): FieldCodec[List[Media]] =
      new FieldCodec[List[Media]](
        fieldId,
        FieldType.Array(FieldType.Media(mimeTypeGroup), minLength, maxLength),
        fieldName,
        None,
        assetsControl.value,
        assetsControl.showCreateEntity.toSet ++ assetsControl.showLinkEntity.toSet ++ assetsControl.settings ++ assetsControl.helpText.toSet
      ) {}

    def entry(
        fieldId: String,
        fieldName: String,
        linkContentTypes: Set[ContentTypeId],
        entryControl: EntryControl = Control.BuiltIn.EntryLinkEditor.entry
    ): FieldCodec[Reference] =
      new FieldCodec[Reference](
        fieldId,
        FieldType.Reference(linkContentTypes),
        fieldName,
        None,
        entryControl.value,
        entryControl.showCreateEntity.toSet ++ entryControl.showLinkEntity.toSet ++ entryControl.settings ++ entryControl.helpText.toList
      ) {}

    def entries(
        fieldId: String,
        fieldName: String,
        linkContentTypes: Set[ContentTypeId],
        minLength: Option[Int] = None,
        maxLength: Option[Int] = None,
        entriesControl: EntriesControl = Control.BuiltIn.EntryLinksEditor.entries
    ): FieldCodec[List[Reference]] =
      new FieldCodec[List[Reference]](
        fieldId,
        FieldType.Array(FieldType.Reference(linkContentTypes), minLength, maxLength),
        fieldName,
        None,
        entriesControl.value,
        entriesControl.showCreateEntity.toSet ++ entriesControl.showLinkEntity.toSet ++ entriesControl.bulkEditing.toSet ++ entriesControl.settings ++ entriesControl.helpText.toSet
      ) {}
