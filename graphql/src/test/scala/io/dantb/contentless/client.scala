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
import edu.gemini.grackle.Ast.Document
import edu.gemini.grackle.GraphQLParser
import edu.gemini.grackle.syntax.*
import io.dantb.contentless.graphql.GraphQLContentAPI
import munit.{CatsEffectSuite, ScalaCheckSuite}
import org.scalacheck.Prop.*

// Examples of user-facing API used for the documentation.
class ClientSpec extends CatsEffectSuite:
  test("TODO acceptance test with real contentful / integration test with stubbed one") {
    // val result = GraphQLContentAPI
    //   .default[IO](GraphQLContentAPI.Config("nlpyjseuka0l", "master", "<TODO>"))
    //   .use { api =>
    //     api.docJson(
    //       GraphQLParser.Document
    //         .parseAll("""
    //       query {
    //           articleCollection(locale: "en-GB", preview: false, limit: 2) {
    //               items {
    //                   sys {
    //                       id
    //                   }
    //                   title
    //                   headerImage {
    //                       url
    //                   }
    //                   displayDate

    //             }
    //         }
    //       }
    //               """)
    //         .right
    //         .get
    //     )
    //   }
    //   .unsafeRunSync()

    // println(s"THE RESULT IS =======> $result")
  }
