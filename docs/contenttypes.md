# Content Types

## Defining content types in code
`ContentType` allows us to codify every aspect of a content type using Scala datatypes.

```scala
trait ContentType[A]:
  def id: ContentTypeId            // unique identifier of this content type
  def contentTypeName: String      // Display name for this content type
  def displayField: Option[String] //
  def description: Option[String]
  def codec: EntryCodec[A]
```

The most important part is the schema definition for this `ContentType`, represented using an `EntryCodec`. This can be constructed through the DSL provided by `io.dantb.contentless.dsl.*`. For example, let's say we want to model articles for a blog involving images, authors and a set of predefined tags. A basic model might be:

```scala mdoc:silent
import io.dantb.contentless.*
import io.dantb.contentless.dsl.*
import java.time.ZonedDateTime

enum Tag(val slug: String):
  case Sport      extends Tag("sport")
  case Politics   extends Tag("politics")
  case Science    extends Tag("science")
  case Technology extends Tag("technology")

def allTags                             = Set(Tag.Sport, Tag.Politics, Tag.Science, Tag.Technology)
def parseTag(slug: String): Option[Tag] = allTags.find(_.slug == slug)

final case class Author(name: String, image: Media, bio: Option[String])
object Author:
  val id: ContentTypeId = ContentTypeId("author")

final case class Article(
    title: String,
    body: RichText.Node,
    tags: Set[Tag],
    authors: Set[Reference],
    coverImage: Option[Media],
    displayDate: ZonedDateTime
)
```

The library types involved here are:
1. `Media` - string wrapper representing a reference to a Contentful asset.
2. `Reference` - string wrapper representing a reference to a Contentful entry.
3. `RichText.Node` - a recursive sum type representing the nodes of a Contentful rich text document.

We define an `EntryCodec[Article]` as follows:

```scala mdoc
import cats.syntax.all.*
import io.dantb.contentless.codecs.EntryCodec

val articleCodec: EntryCodec[Article] =
  (text("title", "Title").required *:
    richText("body", "Body").required *:
    textList("tags", "Tags").required
      .eimap(_.traverse(t => parseTag(t).toRight(s"Invalid tag: $t")).map(_.toSet))(_.map(_.slug).toList) *:
    entries("authors", "Authors", Set(Author.id)).required.eimap(_.toSet.asRight)(_.toList) *:
    asset("coverImage", "Cover Image", Set(MimeTypeGroup.Image)).optional *:
    zonedDateTime("displayDate", "Display Date").required).to[Article]
```

Each field is defined using DSL functions such as `text`, `entries` and `textList` to produce, respectively, an `EntryCodec[String]`, `EntryCodec[Set[Reference]]` and `EntryCodec[Set[Tag]]`. These are composed using the [twiddles](https://github.com/typelevel/twiddles) library to automatically map to and from `Article`. (This is an example of the [Invariant Monoidal](https://typelevel.org/cats/typeclasses/invariantmonoidal.html) pattern, commonly used for codecs.)

`eimap` is used to convert to and from our domain-specific types, for instance from `List[String]` to `Set[Tag]`.

We could additionally define validations and default values on fields:

```scala mdoc
import io.dantb.contentless.Validation.Size

textList(
  "tags", 
  "Tags", 
  defaultValue = Some(List(Tag.Technology.slug)),
  arrayBounds = Size.range(min = 1, max = 3, "Please select 1-3 tags").some
).required
```

Different field types provide different options for customisation. We have tried to reduce the API surface area to only those validations relevant to that particular field type.


## Declaring content types in Contentful
Given a type implementing `ContentType`, we can declare this using the `contentless-management` module. This is backed by a http4s `Client[F]`.

For the above example...

TODO.


## Diffing local content types with Contentful
Examining differences between the code and Contentful models is useful for versioning and automation. It prevents unexpected manual changes to models through the UI so that they are captured fully in code.

TODO.
