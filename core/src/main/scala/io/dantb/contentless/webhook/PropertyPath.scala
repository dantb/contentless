package io.dantb.contentless.webhook
import cats.syntax.all.*

sealed abstract class PropertyPath(val asString: String)

object PropertyPath {
  case object EntityId    extends PropertyPath("sys.id")
  case object Environment extends PropertyPath("sys.environment.sys.id")
  case object ContentType extends PropertyPath("sys.contentType.sys.id")
  case object CreatedBy   extends PropertyPath("sys.createdBy.sys.id")
  case object UpdatedBy   extends PropertyPath("sys.updatedBy.sys.id")

  def fromString(str: String): Option[PropertyPath] =
    str match {
      case "sys.id"                 => PropertyPath.EntityId.some
      case "sys.environment.sys.id" => PropertyPath.Environment.some
      case "sys.contentType.sys.id" => PropertyPath.ContentType.some
      case "sys.createdBy.sys.id"   => PropertyPath.CreatedBy.some
      case "sys.updatedBy.sys.id"   => PropertyPath.UpdatedBy.some
      case _                        => none
    }
}
