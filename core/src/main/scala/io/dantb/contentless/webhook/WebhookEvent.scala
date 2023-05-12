package io.dantb.contentless.webhook
import cats.syntax.all.*

sealed abstract class WebhookEvent(val eventName: String)

object WebhookEvent:
  case object Publish   extends WebhookEvent("publish")
  case object Unpublish extends WebhookEvent("unpublish")
  case object Unarchive extends WebhookEvent("unarchive")
  case object Archive   extends WebhookEvent("archive")
  case object Save      extends WebhookEvent("save")
  case object Create    extends WebhookEvent("create")
  case object Delete    extends WebhookEvent("delete")
  case object Any       extends WebhookEvent("*")

  def fromString(str: String): Option[WebhookEvent] =
    str match
      case "publish"   => WebhookEvent.Publish.some
      case "unpublish" => WebhookEvent.Unpublish.some
      case "unarchive" => WebhookEvent.Unarchive.some
      case "archive"   => WebhookEvent.Archive.some
      case "save"      => WebhookEvent.Save.some
      case "create"    => WebhookEvent.Create.some
      case "delete"    => WebhookEvent.Delete.some
      case "*"         => WebhookEvent.Any.some
      case _           => none
