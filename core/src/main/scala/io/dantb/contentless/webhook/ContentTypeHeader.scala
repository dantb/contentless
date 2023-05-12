package io.dantb.contentless.webhook

/** Contentful give the content type header special treatment. There is a predefined set of allowed content types:
  * https://www.contentful.com/developers/docs/references/content-management-api/#transforming-webhook-calls
  */
sealed abstract case class ContentTypeHeader(key: String)

object ContentTypeHeader {
  object VendorDefault      extends ContentTypeHeader("application/vnd.contentful.management.v1+json")
  object VendorDefaultUtf8  extends ContentTypeHeader("application/vnd.contentful.management.v1+json; charset=utf-8")
  object Json               extends ContentTypeHeader("application/json")
  object JsonUtf8           extends ContentTypeHeader("application/json; charset=utf-8")
  object UrlEncodedForm     extends ContentTypeHeader("application/x-www-form-urlencoded")
  object UrlEncodedFormUtf8 extends ContentTypeHeader("application/x-www-form-urlencoded; charset=utf-8")

  def parse(raw: String): Option[ContentTypeHeader] = raw match {
    case VendorDefault.key      => Some(VendorDefault)
    case VendorDefaultUtf8.key  => Some(VendorDefaultUtf8)
    case Json.key               => Some(Json)
    case JsonUtf8.key           => Some(JsonUtf8)
    case UrlEncodedForm.key     => Some(UrlEncodedForm)
    case UrlEncodedFormUtf8.key => Some(UrlEncodedFormUtf8)
    case _                      => None
  }
}
