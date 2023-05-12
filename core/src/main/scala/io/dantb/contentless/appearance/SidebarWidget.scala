package io.dantb.contentless.appearance

import cats.{Eq, Show}
import cats.syntax.all.*
import io.dantb.contentless.appearance.SidebarWidget.BuiltIn.*

sealed trait SidebarWidget {
  def id: String
  def namespace: String
}

object SidebarWidget {
  given show: Show[SidebarWidget] = Show.show {
    case e: Extension => s"SidebarExtension(id = ${e.id}, namespace = ${e.namespace})"
    case a: App       => s"SidebarApp(id = ${a.id}, namespace = ${a.namespace}"
    case b: BuiltIn   => b.show
  }

  given eq: Eq[SidebarWidget] = Eq.instance { (a, b) =>
    a.id === b.id && a.namespace == b.namespace
  }

  // Taken from https://www.contentful.com/developers/_assets/apps/editor-interfaces/default-sidebar.164c3d2425.json
  val Defaults: List[SidebarWidget] = List(
    Publication,
    Tasks,
    ContentPreview,
    IncomingLinks,
    Translation,
    Versions,
    Users
  )

  final case class Extension(id: String) extends SidebarWidget {
    override val namespace: String = Extension.Namespace
  }
  object Extension {
    val Namespace: String = "extension"
  }

  final case class App(id: String) extends SidebarWidget {
    override val namespace: String = App.Namespace
  }
  object App {
    val Namespace: String = "app"
  }

  // Contentful code with enum in: https://github.com/contentful/contentful-management.js/blob/master/lib/constants/editor-interface-defaults/sidebar-defaults.ts
  sealed abstract case class BuiltIn(id: String) extends SidebarWidget {
    // TODO check whether this or builtin-sidebar (from docs) are more recent version
    final override val namespace: String = BuiltIn.Namespace
  }

  object BuiltIn {
    given showBuiltIn: Show[BuiltIn] = Show.fromToString

    val Namespace: String = "sidebar-builtin"

    object Users          extends BuiltIn("users-widget")
    object ContentPreview extends BuiltIn("content-preview-widget")
    object Translation    extends BuiltIn("translation-widget")
    object IncomingLinks  extends BuiltIn("incoming-links-widget")
    object Publication    extends BuiltIn("publication-widget")
    object Releases       extends BuiltIn("releases-widget")
    object Versions       extends BuiltIn("versions-widget")
    object InfoPanel      extends BuiltIn("info-panel")
    object Jobs           extends BuiltIn("jobs-widget")
    object Tasks          extends BuiltIn("content-workflows-tasks-widget")
    object CommentsPanel  extends BuiltIn("comments-panel")

    def parse(id: String): Option[BuiltIn] = id match {
      case Users.id          => Users.some
      case ContentPreview.id => ContentPreview.some
      case Translation.id    => Translation.some
      case IncomingLinks.id  => IncomingLinks.some
      case Publication.id    => Publication.some
      case Releases.id       => Releases.some
      case Versions.id       => Versions.some
      case InfoPanel.id      => InfoPanel.some
      case Jobs.id           => Jobs.some
      case Tasks.id          => Tasks.some
      case CommentsPanel.id  => CommentsPanel.some
      case _                 => None
    }
  }

  def parse(namespace: String, wid: String): Option[SidebarWidget] = namespace match {
    case BuiltIn.Namespace   => BuiltIn.parse(wid)
    case Extension.Namespace => Some(Extension(wid))
    case App.Namespace       => Some(App(wid))
    case _                   => None
  }

}
