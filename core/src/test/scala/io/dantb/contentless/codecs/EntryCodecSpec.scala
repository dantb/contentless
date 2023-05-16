// /*
//  * Copyright 2023 Typelevel
//  *
//  * Licensed under the Apache License, Version 2.0 (the "License");
//  * you may not use this file except in compliance with the License.
//  * You may obtain a copy of the License at
//  *
//  *     http://www.apache.org/licenses/LICENSE-2.0
//  *
//  * Unless required by applicable law or agreed to in writing, software
//  * distributed under the License is distributed on an "AS IS" BASIS,
//  * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//  * See the License for the specific language governing permissions and
//  * limitations under the License.
//  */

package io.dantb.contentless.codecs

import io.dantb.contentless.{ContentModel, ContentTypeId}
import io.dantb.contentless.dsl.*

class EntryCodecSpec extends munit.FunSuite:

  test("bool field") {
    assertEquals(
      boolean("myField", "My Field", Some(true)).required.schema,
      List()
    )
  }

  test("basic content type with primitives") {
    final case class Foo(t: String, i: Int, b: Boolean, d: Double, lt: Option[String])

    val fooContentType: ContentType[Foo] =
      ContentType(
        ContentTypeId("foo"),
        "Foo",
        Some("t"),
        Some("A foo does foo things"),
        (text("t", "Text field").required *:
          int("i", "Integer field").required *:
          boolean("b", "Boolean field").required *:
          decimal("d", "Decimal field").required *:
          longText("lt", "Optional long text field").optional).to[Foo]
      )

    assertEquals(
      fooContentType.codec.schema,
      List()
    )
  }
