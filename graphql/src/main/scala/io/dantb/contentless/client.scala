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

package io.dantb.contentless.graphql

import cats.MonadThrow
import cats.effect.*
import cats.syntax.all.*
import edu.gemini.grackle.Ast.Document
import edu.gemini.grackle.QueryMinimizer
import io.circe.{Decoder, Json}
import io.circe.syntax.*
import io.dantb.contentless.dsl.{ContentModel, ContentType}
import org.http4s.{AuthScheme, Credentials, MediaType, Method, Request}
import org.http4s.circe.{jsonDecoder, jsonEncoder}
import org.http4s.client.Client
import org.http4s.ember.client.EmberClientBuilder
import org.http4s.headers.{Authorization, `Content-Type`}
import org.http4s.implicits.*
import org.typelevel.log4cats.StructuredLogger

trait GraphQLContentAPI[F[_]]:
  def entry[A: ContentType: Decoder](id: String, model: ContentModel): F[A]
  // Raw JSON when requesting every nested field for this content type. Fails if referenced entries are not present in the model.
  def entryJson[A: ContentType](id: String, model: ContentModel): F[Json]
  def entries[A: ContentType: Decoder](model: ContentModel): F[List[A]]
  def entriesJson[A: ContentType](model: ContentModel): F[List[Json]]
  def docJson(doc: Document): F[Json]

object GraphQLContentAPI:
  final case class Config(space: String, env: String, token: String)

  // def generateDecoder[A: ContentType]: Decoder[A] =

  def default[F[_]: Async: StructuredLogger](conf: Config): Resource[F, GraphQLContentAPI[F]] =
    EmberClientBuilder.default[F].build.map { client =>
      mk(client, conf)
    }

  def mk[F[_]: Concurrent: StructuredLogger](client: Client[F], conf: Config): GraphQLContentAPI[F] =
    new GraphQLContentAPI[F]:

      val ContentfulUrl = uri"https://graphql.contentful.com/content/v1" / "spaces" / conf.space / "environments" / conf.env

      final case class Entries[A](items: List[A])
      given strict[A: Decoder: ContentType]: Decoder[Entries[A]] =
        Decoder.instance(
          _.downField("data").downField(s"${ContentType[A].id.asString}Collection").get[List[A]]("items").map(Entries(_))
        )
      given json[A: ContentType]: Decoder[Entries[Json]] =
        Decoder.instance(
          _.downField("data").downField(s"${ContentType[A].id.asString}Collection").get[List[Json]]("items").map(Entries(_))
        )

      def entry[A: ContentType: Decoder](id: String, model: ContentModel = ContentModel.Empty): F[A] =
        entryJson(id, model).flatMap(
          _.as[Entries[A]].toOption
            .flatMap(_.items.headOption)
            .map(_.pure[F])
            .getOrElse(raise(s"No entries of content type ${ContentType[A].id.asString} with id $id"))
        )

      def entryJson[A: ContentType](id: String, model: ContentModel = ContentModel.Empty): F[Json] =
        singleEntryQueryString[A](id, model).fold(
          MonadThrow[F].raiseError,
          queryString => client.expect[Json](mkRequest(queryString))
        )

      def entries[A: ContentType: Decoder](model: ContentModel = ContentModel.Empty): F[List[A]] =
        entriesJson(model).flatMap(
          _.flatTraverse(
            _.as[A].fold(
              e =>
                StructuredLogger[F]
                  .warn(s"Omitting ${ContentType[A].id.asString} entry since parsing failed with $e")
                  .as(Nil),
              List(_).pure[F]
            )
          )
        )

      def entriesJson[A: ContentType](model: ContentModel): F[List[Json]] =
        contentTypeQueryString[A](model).fold(
          MonadThrow[F].raiseError,
          queryString =>
            client
              .expect[Json](mkRequest(queryString))
              .flatMap(_.as[Entries[Json]].map(_.items).fold(raise[List[Json]](_), _.pure[F]))
        )

      def docJson(doc: Document): F[Json] = client.expect[Json](mkRequest(QueryMinimizer.minimizeDocument(doc)))

      def raise[A](msg: String): F[A]   = raise(new Exception(msg))
      def raise[A](ex: Exception): F[A] = MonadThrow[F].raiseError(ex)

      def mkRequest(queryString: String): Request[F] =
        Request[F](Method.POST, ContentfulUrl)
          .withContentType(`Content-Type`(MediaType.application.json))
          .withEntity(Json.obj("query" -> queryString.asJson))
          .withHeaders(Authorization(Credentials.Token(AuthScheme.Bearer, conf.token)))
