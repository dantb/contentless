/*
 * Copyright 2023 Typelevel
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.typelevel.contentless

import munit.CatsEffectSuite
import java.time.ZonedDateTime

class GraphQLSuite extends CatsEffectSuite {

  final case class Params(skip: Int, limit: Int, locale: String, preview: Boolean)

  final case class Article(title: String, updatedAt: ZonedDateTime, label: Label, region: Region)

  sealed trait Label
  object Label {
    case object BreakingNews extends Label
    case object Exclusive extends Label
  }

  final case class Region(slug: String, label: String)

  test("GQL Main should exit succesfully") {
    val main = Main.run.attempt
    assertIO(main, Right(()))
  }

}
