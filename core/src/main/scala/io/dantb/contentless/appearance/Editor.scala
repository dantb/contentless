package io.dantb.contentless.appearance

import cats.{Eq, Show}
import cats.syntax.all.*
import io.dantb.contentless.appearance.Editor.BuiltIn.EntryEditor

sealed trait Editor {
  def id: String
  def namespace: String
  def disabled: Boolean
}

object Editor {
  // Taken from: https://www.contentful.com/developers/_assets/apps/editor-interfaces/default-editors.3870e02a40.json
  val Default: Set[Editor] = Set(EntryEditor(disabled = false))

  implicit val show: Show[Editor] = Show.show {
    case e: Extension => s"EditorExtension(id = ${e.id}, namespace = ${e.namespace}, disabled = ${e.disabled}"
    case a: App       => s"EditorApp(id = ${a.id}, namespace = ${a.namespace}, disabled = ${a.disabled})"
    case b: BuiltIn   => b.show
  }

  implicit val eq: Eq[Editor] = Eq.instance { (a, b) =>
    a.id === b.id && a.namespace === b.namespace && a.disabled === b.disabled
  }

  final case class Extension(id: String, disabled: Boolean = false) extends Editor {
    override val namespace: String = Extension.Namespace
  }

  object Extension {
    val Namespace = "extension"
  }

  final case class App(id: String, disabled: Boolean = false) extends Editor {
    override val namespace: String = App.Namespace
  }
  object App {
    val Namespace = "app"
  }

  sealed abstract class BuiltIn(val id: String) extends Editor {
    final override def namespace: String = BuiltIn.Namespace
  }
  object BuiltIn {
    val Namespace = "editor-builtin"

    implicit val showBuiltIn: Show[BuiltIn] = Show.show {
      case EntryEditor(disabled)      => s"EntryEditor(disabled = $disabled)"
      case ReferencesEditor(disabled) => s"ReferencesEditor(disabled = $disabled)"
      case TagsEditor(disabled)       => s"TagsEditor(disabled = $disabled)"
    }

    final case class EntryEditor(disabled: Boolean) extends BuiltIn(EntryEditor.Id)
    object EntryEditor {
      val Id: String = "default-editor"
    }
    final case class ReferencesEditor(disabled: Boolean) extends BuiltIn("reference-tree")
    object ReferencesEditor {
      val Id: String = "reference-tree"
    }
    final case class TagsEditor(disabled: Boolean) extends BuiltIn("tags-editor")
    object TagsEditor {
      val Id: Any = "tags-editor"
    }

    def parse(id: String, disabled: Boolean): Option[BuiltIn] = id match {
      case EntryEditor.Id      => EntryEditor(disabled).some
      case ReferencesEditor.Id => ReferencesEditor(disabled).some
      case TagsEditor.Id       => TagsEditor(disabled).some
      case _                   => None
    }
  }

  def parse(namespace: String, wid: String, disabled: Boolean): Option[Editor] = namespace match {
    case BuiltIn.Namespace   => BuiltIn.parse(wid, disabled)
    case App.Namespace       => Some(App(wid))
    case Extension.Namespace => Some(Extension(wid))
    case _                   => None
  }

}
