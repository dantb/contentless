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

package io.dantb.contentless

import java.time.ZonedDateTime

import edu.gemini.grackle.Ast._
import io.circe._

object core {

  trait Entry[A]

  // TODO use newtypes, too many strings makes me nervous
  final case class Sys(
      id: String,
      spaceId: String,
      envId: String,
      publishedAt: Option[ZonedDateTime],
      firstPublishedAt: Option[ZonedDateTime],
      publishedVersion: Option[String]
  )

  sealed trait Reference[A]
  object Reference {
    final case class Flat[A](sys: Sys) extends Reference[A]
    final case class Deep[A](sysId: String, a: A)
  }

  final case class Params(skip: Int, limit: Int, locale: String, preview: Boolean)

}

// TODO: let the user specify a depth to go to for generating subqueries?
object graphql {

  import core._

  // TODO make depth min 0 (direct fields only) and max 10

  // Note that there's no way to enforce that depth corresponds to Deep in the ADT above...
  // We could have depth 2 but only Flat values at compile time. So it would have to be handled by users
  // in a pattern match...
  // I guess we could make some utility to get rid of the optionality? Hard to generalise...
  // Or we could just have optics to access fields deep in the structure.
  // E.g. you'd say article.region.slug and have the library figure out how to project it...
  // It we return an Option[String] though, when it should just be String.
  // Maybe this isn't worth the complexity I'm adding?
  // Thing is, we can always go from normalised to denormalised. The other way round is the problem.
  // So given the above representation we could generate the existing internals for EntryCodecs
  // Wait... Could we use a dependent type to resolve to a particular depth? So given an A, we can have
  // a type A.B which is A with depth 1... A.B.C which is A with depth 2. They would have hard coded "Deep"
  // values rather than the flat option.
  // could we just use inheritance?

  // This might be overcomplicating. The reason we use Invariant monoidal is because it allows us
  // to both generate and parse, GET and PUT, entries. We might still actually need the REST client, which may
  // be a ballache to rewrite.

  // Maybe start with what we have, and when coming to rest client, think about the core model again?
  // or would that waste time?
  def getEntry[F[_], A: Entry](id: String, depth: Int): F[Option[A]] = ???

  // The user wouldn't need to write the decoder... We'd generate it based on the Entry instance.
  // We could generate it to different depths...
  // But still have thr weirdness that mandatory fields won't look mandatory
  def getEntry[F[_], A: Entry: Decoder](id: String): F[Option[A]] = ???

  def getEntryDoc[F[_], A: Entry: Decoder](doc: Document): F[Option[A]] = ???

  def getEntries[F[_], A: Entry: Decoder](params: Params): F[List[A]] = ???

  object internal {}

}
