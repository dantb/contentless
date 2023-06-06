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
