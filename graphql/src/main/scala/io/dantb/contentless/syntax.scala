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

import cats.syntax.all.*
import edu.gemini.grackle.{GraphQLParser, Schema}
import edu.gemini.grackle.Ast.Document
import org.typelevel.literally.Literally

object syntax:

  extension (inline ctx: StringContext) inline def doc(inline args: Any*): Document = ${ DocumentLiteral('ctx, 'args) }

  object DocumentLiteral extends Literally[Document]:
    def validate(s: String)(using Quotes) =
      GraphQLParser.Document
        .parseAll(s)
        .bimap(
          pf => show"Invalid document: $pf",
          _ => '{ GraphQLParser.Document.parseAll(${ Expr(s) }).getOrElse(???) }
        )
