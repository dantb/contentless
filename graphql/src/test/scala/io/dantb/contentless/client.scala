/*
 * Copyright 2023 Daniel Tattan-Birch
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

package io.dantb.contentless

import java.time.ZonedDateTime

import cats.effect.IO
import ciris.*
import edu.gemini.grackle.Ast.Document
import edu.gemini.grackle.GraphQLParser
import edu.gemini.grackle.syntax.*
import io.circe.Json
import io.dantb.contentless.graphql.GraphQLContentAPI
import munit.{CatsEffectSuite, ScalaCheckSuite}
import org.scalacheck.Prop.*
import org.typelevel.log4cats.StructuredLogger
import org.typelevel.log4cats.noop.NoOpLogger

// Examples of user-facing API used for the documentation.
class ClientSpec extends CatsEffectSuite:
  given StructuredLogger[IO] = NoOpLogger[IO]

  test("TODO acceptance test with real contentful / integration test with stubbed one") {
    val config: IO[GraphQLContentAPI.Config] = (for
      key   <- env("CONTENTFUL_KEY")
      space <- env("CONTENTFUL_SPACE")
      env   <- env("CONTENTFUL_ENV")
    yield GraphQLContentAPI.Config(space, env, key)).load[IO]

    val result = config.flatMap { conf =>
      GraphQLContentAPI.default[IO](conf).use { api =>
        api.docJson(
          doc"""
            query {
                articleCollection(locale: "en-GB", preview: false, limit: 2) {
                    items {
                        sys {
                            id
                        }
                        title
                        headerImage {
                            url
                        }
                        displayDate

                  }
              }
            }
              """
        )
      }
    }

    assertIO_(result.void)
  }
