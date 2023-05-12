package io.dantb.contentless.webhook

sealed abstract case class Method(asString: String)

object Method:
  object Post   extends Method("POST")
  object Get    extends Method("GET")
  object Put    extends Method("PUT")
  object Patch  extends Method("PATCH")
  object Delete extends Method("DELETE")

  def parse(raw: String): Option[Method] = raw match
    case Post.asString   => Some(Post)
    case Get.asString    => Some(Get)
    case Put.asString    => Some(Put)
    case Patch.asString  => Some(Patch)
    case Delete.asString => Some(Delete)
    case _               => None
