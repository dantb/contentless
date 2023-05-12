package io.dantb.contentless.appearance

import cats.{Eq, Show}
import cats.kernel.Monoid
import cats.syntax.all.*
import io.dantb.contentless.ContentTypeId
import io.dantb.contentless.instances.*

// https://www.contentful.com/developers/docs/extensibility/app-framework/editor-interfaces/
final case class EditorInterface(
    editors: Set[Editor],
    sidebarWidgets: List[SidebarWidget],
    controls: Set[FieldControl]
)

object EditorInterface {
  implicit val show: Show[EditorInterface] = Show.show { ei =>
    show"EditorInterface(editors = ${ei.editors}, sidebarWidgets = ${ei.sidebarWidgets}, controls = ${ei.controls})"
  }

  implicit val eq: Eq[EditorInterface] = Eq.instance { (a, b) =>
    eqSet[Editor].eqv(a.editors, b.editors) &&
    a.sidebarWidgets === b.sidebarWidgets &&
    eqSet[FieldControl].eqv(a.controls, b.controls)
  }

  def empty: EditorInterface = EditorInterface(Set.empty, Nil, Set.empty)

  implicit val monoid: Monoid[EditorInterface] = new Monoid[EditorInterface] {
    override def empty: EditorInterface = EditorInterface.empty
    override def combine(x: EditorInterface, y: EditorInterface): EditorInterface =
      EditorInterface(x.editors ++ y.editors, x.sidebarWidgets ++ y.sidebarWidgets, x.controls ++ y.controls)
  }
}

final case class VersionedEditorInterface(
    version: Int,
    contentTypeId: ContentTypeId,
    editorInterface: EditorInterface
)

object VersionedEditorInterface {
  implicit val show: Show[VersionedEditorInterface] = Show.show { vei =>
    show"VersionedEditorInterface(version = ${vei.version}, contentType = ${vei.contentTypeId.asString}, editorInterface = ${vei.editorInterface})"
  }

  implicit val eq: Eq[VersionedEditorInterface] = Eq.instance { (a, b) =>
    a.version === b.version && a.contentTypeId === b.contentTypeId && a.editorInterface === b.editorInterface
  }
}
