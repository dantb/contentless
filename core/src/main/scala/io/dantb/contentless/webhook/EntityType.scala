package io.dantb.contentless.webhook
import cats.syntax.all.*

sealed abstract class EntityType(val typeName: String)

object EntityType {
  case object Entry       extends EntityType("Entry")
  case object Asset       extends EntityType("Asset")
  case object ContentType extends EntityType("ContentType")
  case object Any         extends EntityType("*")

  def fromString(str: String): Option[EntityType] =
    str match {
      case "Entry"       => Entry.some
      case "Asset"       => Asset.some
      case "ContentType" => ContentType.some
      case "*"           => Any.some
      case _             => none
    }
}
