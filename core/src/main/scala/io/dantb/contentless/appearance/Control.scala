package io.dantb.contentless.appearance

import cats.{Eq, Show}
import cats.syntax.all.*
import io.dantb.contentless.appearance.FieldControlSetting.{CustomSetting, HelpText}
import io.dantb.contentless.appearance.FieldControlSetting.Boolean.{FalseLabel, TrueLabel}
import io.dantb.contentless.appearance.FieldControlSetting.DatePicker.{ClockType, Format}
import io.dantb.contentless.appearance.FieldControlSetting.LinksEditor.{
  BulkEditing,
  ShowCreateEntityAction,
  ShowLinkEntityAction
}
import io.dantb.contentless.appearance.FieldControlSetting.Rating.Stars
import io.dantb.contentless.instances.eqSet

final case class FieldControl(
    fieldId: String,
    control: Control,
    settings: Set[FieldControlSetting]
)

object FieldControl:
  given show: Show[FieldControl] = Show.show { fc =>
    show"FieldControl(fieldId = ${fc.fieldId}, control = ${fc.control}, settings = ${fc.settings})"
  }

  given eq: Eq[FieldControl] = Eq.instance { (a, b) =>
    a.fieldId === b.fieldId && a.control === b.control && eqSet[FieldControlSetting].eqv(a.settings, b.settings)
  }

sealed trait Control:
  def id: String
  def namespace: String

object Control:
  given show: Show[Control] = Show.show {
    case e: Extension => s"ControlExtension(id = ${e.id}, namespace = ${e.namespace})"
    case a: App       => s"ControlApp(id = ${a.id}, namespace = ${a.namespace}"
    case b: BuiltIn   => b.show
  }

  given eq: Eq[Control] = Eq.instance { (a, b) =>
    a.id === b.id && a.namespace === b.namespace
  }

  final case class Extension(id: String) extends Control:
    override val namespace: String                     = Extension.Namespace
    def boolean(settings: CustomSetting*): BoolControl = new BoolControl(this, settings.toSet) {}
    def longText(settings: CustomSetting*): LongTextControl =
      new LongTextControl(this, settings.toSet) {}
    def text(settings: CustomSetting*): TextControl =
      new TextControl(this, settings.toSet) {}
    def integer(settings: CustomSetting*): IntControl = new IntControl(this, settings.toSet) {}
    def number(settings: CustomSetting*): NumControl  = new NumControl(this, settings.toSet) {}
    def json(settings: CustomSetting*): JsonControl   = new JsonControl(this, settings.toSet) {}
    def dateTime(settings: CustomSetting*): DateTimeControl =
      new DateTimeControl(this, settings.toSet) {}
    def location(settings: CustomSetting*): LocationControl =
      new LocationControl(this, settings.toSet) {}
    def richText(settings: CustomSetting*): RichTextControl =
      new RichTextControl(this, settings.toSet) {}
    def textList(settings: CustomSetting*): TextListControl =
      new TextListControl(this, settings.toSet) {}
    def asset(settings: CustomSetting*): AssetControl = new AssetControl(this, settings.toSet) {}
    def entry(settings: CustomSetting*): EntryControl = new EntryControl(this, settings.toSet) {}
    def entries(settings: CustomSetting*): EntriesControl =
      new EntriesControl(this, settings.toSet) {}

  object Extension:
    val Namespace = "extension"

  final case class App(id: String) extends Control:
    override val namespace: String                     = App.Namespace
    def boolean(settings: CustomSetting*): BoolControl = new BoolControl(this, settings.toSet) {}
    def longText(settings: CustomSetting*): LongTextControl =
      new LongTextControl(this, settings.toSet) {}
    def text(settings: CustomSetting*): TextControl =
      new TextControl(this, settings.toSet) {}
    def integer(settings: CustomSetting*): IntControl = new IntControl(this, settings.toSet) {}
    def number(settings: CustomSetting*): NumControl  = new NumControl(this, settings.toSet) {}
    def json(settings: CustomSetting*): JsonControl   = new JsonControl(this, settings.toSet) {}
    def dateTime(settings: CustomSetting*): DateTimeControl =
      new DateTimeControl(this, settings.toSet) {}
    def location(settings: CustomSetting*): LocationControl =
      new LocationControl(this, settings.toSet) {}
    def richText(settings: CustomSetting*): RichTextControl =
      new RichTextControl(this, settings.toSet) {}
    def textList(settings: CustomSetting*): TextListControl =
      new TextListControl(this, settings.toSet) {}
    def asset(settings: CustomSetting*): AssetControl = new AssetControl(this, settings.toSet) {}
    def entry(settings: CustomSetting*): EntryControl = new EntryControl(this, settings.toSet) {}
    def entries(settings: CustomSetting*): EntriesControl =
      new EntriesControl(this, settings.toSet) {}

  object App:
    val Namespace = "app"

  sealed abstract case class BuiltIn(id: String) extends Control:
    final override def namespace: String = BuiltIn.Namespace

  object BuiltIn:
    given showBuiltIn: Show[BuiltIn] = Show.fromToString

    val Namespace: String = "builtin"

    object AssetLinkEditor extends BuiltIn("assetLinkEditor"):
      def asset: AssetControl = new AssetControl(this) {}

    object AssetLinksEditor extends BuiltIn("assetLinksEditor"):
      def assets: AssetsControl = new AssetsControl(this) {}
    object AssetGalleryEditor extends BuiltIn("assetGalleryEditor"):
      def assets: AssetsControl = new AssetsControl(this) {}

    object Boolean extends BuiltIn("boolean"):
      def boolean: BoolControl = new BoolControl(this) {}

    object DatePicker extends BuiltIn("datePicker"):
      def dateTime: DateTimeControl = new DateTimeControl(this) {}

    object EntryLinkEditor extends BuiltIn("entryLinkEditor"):
      def entry: EntryControl = new EntryControl(this) {}

    object EntryLinksEditor extends BuiltIn("entryLinksEditor"):
      def entries: EntriesControl = new EntriesControl(this) {}

    object EntryCardEditor extends BuiltIn("entryCardEditor"):
      def entry: EntryControl = new EntryControl(this) {}

    object EntryCardsEditor extends BuiltIn("entryCardsEditor"):
      def entries: EntriesControl = new EntriesControl(this) {}

    object NumberEditor extends BuiltIn("numberEditor"):
      def integer: IntControl = new IntControl(this) {}
      def number: NumControl  = new NumControl(this) {}

    object Rating extends BuiltIn("rating"):
      def integer: IntControl = new IntControl(this) {}
      def number: NumControl  = new NumControl(this) {}

    object LocationEditor extends BuiltIn("locationEditor"):
      def location: LocationControl = new LocationControl(this) {}

    object ObjectEditor extends BuiltIn("objectEditor"):
      def json: JsonControl = new JsonControl(this) {}

    object UrlEditor extends BuiltIn("urlEditor"):
      def text: TextControl = new TextControl(this) {}

    object SlugEditor extends BuiltIn("slugEditor"):
      def text: TextControl = new TextControl(this) {}

    object ListInput extends BuiltIn("listInput"):
      def textList: TextListControl = new TextListControl(this) {}

    object Checkbox extends BuiltIn("checkbox"):
      def textList: TextListControl = new TextListControl(this) {}

    object TagEditor extends BuiltIn("tagEditor"):
      def textList: TextListControl = new TextListControl(this) {}

    object MultipleLine extends BuiltIn("multipleLine"):
      def longText: LongTextControl = new LongTextControl(this) {}

    object Markdown extends BuiltIn("markdown"):
      def longText: LongTextControl = new LongTextControl(this) {}

    object SingleLine extends BuiltIn("singleLine"):
      def text: TextControl         = new TextControl(this) {}
      def longText: LongTextControl = new LongTextControl(this) {}

    object Dropdown extends BuiltIn("dropdown"):
      def text: TextControl         = new TextControl(this) {}
      def longText: LongTextControl = new LongTextControl(this) {}
      def integer: IntControl       = new IntControl(this) {}
      def number: NumControl        = new NumControl(this) {}

    object Radio extends BuiltIn("radio"):
      def text: TextControl         = new TextControl(this) {}
      def longText: LongTextControl = new LongTextControl(this) {}
      def integer: IntControl       = new IntControl(this) {}
      def number: NumControl        = new NumControl(this) {}

    object RichTextEditor extends BuiltIn("richTextEditor"):
      def richText: RichTextControl = new RichTextControl(this) {}

    def parse(id: String): Option[BuiltIn] = id match
      case AssetLinkEditor.id    => AssetLinkEditor.some
      case AssetLinksEditor.id   => AssetLinksEditor.some
      case AssetGalleryEditor.id => AssetGalleryEditor.some
      case Boolean.id            => Boolean.some
      case DatePicker.id         => DatePicker.some
      case EntryLinkEditor.id    => EntryLinkEditor.some
      case EntryLinksEditor.id   => EntryLinksEditor.some
      case EntryCardEditor.id    => EntryCardEditor.some
      case EntryCardsEditor.id   => EntryCardsEditor.some
      case NumberEditor.id       => NumberEditor.some
      case Rating.id             => Rating.some
      case LocationEditor.id     => LocationEditor.some
      case ObjectEditor.id       => ObjectEditor.some
      case UrlEditor.id          => UrlEditor.some
      case SlugEditor.id         => SlugEditor.some
      case ListInput.id          => ListInput.some
      case Checkbox.id           => Checkbox.some
      case TagEditor.id          => TagEditor.some
      case MultipleLine.id       => MultipleLine.some
      case Markdown.id           => Markdown.some
      case SingleLine.id         => SingleLine.some
      case Dropdown.id           => Dropdown.some
      case Radio.id              => Radio.some
      case RichTextEditor.id     => RichTextEditor.some
      case _                     => None

  def parse(namespace: String, wid: String): Option[Control] = namespace match
    case BuiltIn.Namespace   => BuiltIn.parse(wid)
    case Extension.Namespace => Some(Extension(wid))
    case App.Namespace       => Some(App(wid))
    case _                   => None

  // There is a many-many relationship between controls and field types for which those controls are applicable.
  // It feels cleaner to represent this through composition rather than a diamond inheritance hierarchy.
  // The types below are used to make only valid combinations representable in the DSL.
  // Default settings are documented here: https://contentful.github.io/contentful-management.js/contentful-management/7.27.0/globals.html#defaults_settings
  sealed abstract case class BoolControl(
      value: Control,
      settings: Set[CustomSetting] = Set.empty[CustomSetting],
      trueLabel: Option[TrueLabel] = Some(TrueLabel("Yes")),
      falseLabel: Option[FalseLabel] = Some(FalseLabel("No")),
      helpText: Option[HelpText] = None
  ):
    private def copy(
        trueLabel: Option[TrueLabel] = this.trueLabel,
        falseLabel: Option[FalseLabel] = this.falseLabel,
        helpText: Option[HelpText] = this.helpText
    ): BoolControl = new BoolControl(value, settings, trueLabel, falseLabel, helpText) {}

    def withTrueLabel(label: String): BoolControl   = copy(trueLabel = Some(TrueLabel(label)))
    def withFalseLabel(label: String): BoolControl  = copy(falseLabel = Some(FalseLabel(label)))
    def withHelpText(helpText: String): BoolControl = copy(helpText = HelpText(helpText).some)
    def removeTrueLabel(): BoolControl              = copy(trueLabel = None)
    def removeFalseLabel(): BoolControl             = copy(falseLabel = None)

  sealed abstract case class LongTextControl(
      value: Control,
      settings: Set[CustomSetting] = Set.empty[CustomSetting],
      helpText: Option[HelpText] = None
  ):
    def withHelpText(helpText: String): LongTextControl =
      new LongTextControl(value, settings, HelpText(helpText).some) {}

  sealed abstract case class TextControl(
      value: Control,
      settings: Set[CustomSetting] = Set.empty[CustomSetting],
      helpText: Option[HelpText] = None
  ):
    def withHelpText(helpText: String): TextControl = new TextControl(value, settings, HelpText(helpText).some) {}

  sealed abstract case class IntControl(
      value: Control,
      settings: Set[CustomSetting] = Set.empty[CustomSetting],
      stars: Option[Stars] = None,
      helpText: Option[HelpText] = None
  ):
    def withStars(stars: Int): IntControl = new IntControl(value, settings, Stars(stars).some) {}
    def withHelpText(helpText: String): IntControl =
      new IntControl(value, settings, stars, HelpText(helpText).some) {}

  sealed abstract case class NumControl(
      value: Control,
      settings: Set[CustomSetting] = Set.empty[CustomSetting],
      stars: Option[Stars] = None,
      helpText: Option[HelpText] = None
  ):
    def withStars(stars: Int): NumControl = new NumControl(value, settings, Stars(stars).some) {}
    def withHelpText(helpText: String): NumControl =
      new NumControl(value, settings, stars, HelpText(helpText).some) {}

  sealed abstract case class JsonControl(
      value: Control,
      settings: Set[CustomSetting] = Set.empty[CustomSetting],
      helpText: Option[HelpText] = None
  ):
    def withHelpText(helpText: String): JsonControl = new JsonControl(value, settings, HelpText(helpText).some) {}

  sealed abstract case class DateTimeControl(
      value: Control,
      settings: Set[CustomSetting] = Set.empty[CustomSetting],
      format: Option[Format] = Some(Format.Time),
      clockType: Option[ClockType] = Some(ClockType.TwentyFourHour),
      helpText: Option[HelpText] = None
  ):
    private def copy(
        format: Option[Format] = this.format,
        clockType: Option[ClockType] = this.clockType,
        helpText: Option[HelpText] = this.helpText
    ): DateTimeControl = new DateTimeControl(value, settings, format, clockType, helpText) {}

    def zoned: DateTimeControl                           = withFormat(FieldControlSetting.DatePicker.Format.TimeZ)
    def withFormat(format: Format): DateTimeControl      = copy(format = Some(format))
    def withClockType(clock: ClockType): DateTimeControl = copy(clockType = Some(clock))
    def withHelpText(helpText: String): DateTimeControl  = copy(helpText = HelpText(helpText).some)
    def removeFormat(): DateTimeControl                  = copy(format = None)
    def removeClockType(): DateTimeControl               = copy(clockType = None)

  object DateTimeControl:
    val ZonedDefault = Control.BuiltIn.DatePicker.dateTime.zoned
    val LocalDefault = Control.BuiltIn.DatePicker.dateTime

  sealed abstract case class LocationControl(
      value: Control,
      settings: Set[CustomSetting] = Set.empty[CustomSetting],
      helpText: Option[HelpText] = None
  ):
    def withHelpText(helpText: String): LocationControl =
      new LocationControl(value, settings, HelpText(helpText).some) {}

  sealed abstract case class RichTextControl(
      value: Control,
      settings: Set[CustomSetting] = Set.empty[CustomSetting],
      helpText: Option[HelpText] = None
  ):
    def withHelpText(helpText: String): RichTextControl =
      new RichTextControl(value, settings, HelpText(helpText).some) {}

  sealed abstract case class TextListControl(
      value: Control,
      settings: Set[CustomSetting] = Set.empty[CustomSetting],
      helpText: Option[HelpText] = None
  ):
    def withHelpText(helpText: String): TextListControl =
      new TextListControl(value, settings, HelpText(helpText).some) {}

  sealed abstract case class AssetControl(
      value: Control,
      settings: Set[CustomSetting] = Set.empty[CustomSetting],
      showCreateEntity: Option[ShowCreateEntityAction] = Some(ShowCreateEntityAction(true)),
      showLinkEntity: Option[ShowLinkEntityAction] = Some(ShowLinkEntityAction(true)),
      helpText: Option[HelpText] = None
  ):
    private def copy(
        showCreateEntity: Option[ShowCreateEntityAction] = this.showCreateEntity,
        showLinkEntity: Option[ShowLinkEntityAction] = this.showLinkEntity,
        helpText: Option[HelpText] = this.helpText
    ): AssetControl = new AssetControl(value, settings, showCreateEntity, showLinkEntity, helpText) {}

    def withShowCreateEntity(showCreateEntity: Boolean): AssetControl =
      copy(showCreateEntity = Some(ShowCreateEntityAction(showCreateEntity)))
    def withShowLinkEntity(showLinkEntity: Boolean): AssetControl =
      copy(showLinkEntity = Some(ShowLinkEntityAction(showLinkEntity)))
    def withHelpText(helpText: String): AssetControl = copy(helpText = HelpText(helpText).some)
    def removeShowCreateEntity(): AssetControl       = copy(showCreateEntity = None)
    def removeShowLinkEntity(): AssetControl         = copy(showLinkEntity = None)

  // Indentical in configuration. However, users should not be able to set an assets field with an AssetControl, or vice versa.
  opaque type AssetsControl = AssetControl
  object AssetsControl:
    export AssetControl.*
    extension (a: AssetsControl)
      def value: Control                                                = a.value
      def settings: Set[CustomSetting]                                  = a.settings
      def showCreateEntity: Option[ShowCreateEntityAction]              = a.showCreateEntity
      def showLinkEntity: Option[ShowLinkEntityAction]                  = a.showLinkEntity
      def helpText: Option[HelpText]                                    = a.helpText
      def withShowCreateEntity(showCreateEntity: Boolean): AssetControl = a.withShowCreateEntity(showCreateEntity)
      def withShowLinkEntity(showLinkEntity: Boolean): AssetControl     = a.withShowLinkEntity(showLinkEntity)
      def withHelpText(helpText: String): AssetControl                  = a.withHelpText(helpText)
      def removeShowCreateEntity(): AssetControl                        = a.removeShowCreateEntity()
      def removeShowLinkEntity(): AssetControl                          = a.removeShowLinkEntity()

  sealed abstract case class EntryControl(
      value: Control,
      settings: Set[CustomSetting] = Set.empty[CustomSetting],
      showCreateEntity: Option[ShowCreateEntityAction] = Some(ShowCreateEntityAction(true)),
      showLinkEntity: Option[ShowLinkEntityAction] = Some(ShowLinkEntityAction(true)),
      helpText: Option[HelpText] = None
  ):
    private def copy(
        showCreateEntity: Option[ShowCreateEntityAction] = this.showCreateEntity,
        showLinkEntity: Option[ShowLinkEntityAction] = this.showLinkEntity,
        helpText: Option[HelpText] = this.helpText
    ): EntryControl = new EntryControl(value, settings, showCreateEntity, showLinkEntity, helpText) {}

    def withShowCreateEntity(showCreateEntity: Boolean): EntryControl =
      copy(showCreateEntity = Some(ShowCreateEntityAction(showCreateEntity)))
    def withShowLinkEntity(showLinkEntity: Boolean): EntryControl =
      copy(showLinkEntity = Some(ShowLinkEntityAction(showLinkEntity)))
    def withHelpText(helpText: String): EntryControl = copy(helpText = HelpText(helpText).some)
    def removeShowCreateEntity(): EntryControl       = copy(showCreateEntity = None)
    def removeShowLinkEntity(): EntryControl         = copy(showLinkEntity = None)

  sealed abstract case class EntriesControl(
      value: Control,
      settings: Set[CustomSetting] = Set.empty[CustomSetting],
      showCreateEntity: Option[ShowCreateEntityAction] = Some(ShowCreateEntityAction(true)),
      showLinkEntity: Option[ShowLinkEntityAction] = Some(ShowLinkEntityAction(true)),
      bulkEditing: Option[BulkEditing] = Some(BulkEditing(false)),
      helpText: Option[HelpText] = None
  ):
    private def copy(
        showCreateEntity: Option[ShowCreateEntityAction] = this.showCreateEntity,
        showLinkEntity: Option[ShowLinkEntityAction] = this.showLinkEntity,
        bulkEditing: Option[BulkEditing] = this.bulkEditing,
        helpText: Option[HelpText] = this.helpText
    ): EntriesControl =
      new EntriesControl(value, settings, showCreateEntity, showLinkEntity, bulkEditing, helpText) {}

    def withShowCreateEntity(showCreateEntity: Boolean): EntriesControl =
      copy(showCreateEntity = Some(ShowCreateEntityAction(showCreateEntity)))
    def withShowLinkEntity(showLinkEntity: Boolean): EntriesControl =
      copy(showLinkEntity = Some(ShowLinkEntityAction(showLinkEntity)))
    def withBulkEditing(bulkEditing: Boolean): EntriesControl =
      copy(bulkEditing = Some(BulkEditing(bulkEditing)))
    def withHelpText(helpText: String): EntriesControl = copy(helpText = HelpText(helpText).some)
    def removeShowCreateEntity(): EntriesControl       = copy(showCreateEntity = None)
    def removeShowLinkEntity(): EntriesControl         = copy(showLinkEntity = None)
    def removeBulkEditing(): EntriesControl            = copy(bulkEditing = None)
