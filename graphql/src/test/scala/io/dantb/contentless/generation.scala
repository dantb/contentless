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
import edu.gemini.grackle.Ast.Document
import edu.gemini.grackle.GraphQLParser
import edu.gemini.grackle.QueryMinimizer.minimizeDocument as minDoc
import io.dantb.contentless.dsl.ContentModel
import io.dantb.contentless.graphql.*
import io.dantb.contentless.syntax.*
import munit.ScalaCheckSuite
import org.scalacheck.Prop.*

// Examples of user-facing API used for the documentation.
// TODO: CHECK that these all actually work, once the example models have been declared for real
class GenerationSpec extends ScalaCheckSuite:

  test("Generate query string for authors") {
    val result = contentTypeQueryString[Author]()

    assertEquals(result, Right(minDoc(authors)))
  }

  test("Generate query string for specific author") {
    val result = singleEntryQueryString[Author](id = "abc", ContentModel.Empty)

    assertEquals(result, Right(minDoc(author)))
  }

  test("Generate query string for article with nested authors") {
    val model  = ContentModel(List(Author.ct, Article.ct))
    val result = contentTypeQueryString[Article](model)

    assertEquals(result, Right(minDoc(article)))
  }

  def article: Document = doc"""
{
  articleCollection(skip: 0, limit: 10, preview: false, locale: "en-GB") {
    items {
      sys {
        id
        publishedAt
        firstPublishedAt
        publishedVersion
      }
      authorsCollection {
        items {
          sys {
            id
            publishedAt
            firstPublishedAt
            publishedVersion
          }
          bio
          image {
            sys {
              id
              publishedAt
              firstPublishedAt
              publishedVersion
            }
            description
            height
            title
            url
            width
          }
          name
        }
      }
      body {
        json
      }
      coverImage {
        sys {
          id
          publishedAt
          firstPublishedAt
          publishedVersion
        }
        description
        height
        title
        url
        width
      }
      displayDate
      tags
      title
    }
  }
}
  """

  def authors: Document = doc"""
{
  authorCollection(skip: 0, limit: 10, preview: false, locale: "en-GB") {
    items {
      sys {
        id
        publishedAt
        firstPublishedAt
        publishedVersion
      }
      bio
      image {
        sys {
          id
          publishedAt
          firstPublishedAt
          publishedVersion
        }
        description
        height
        title
        url
        width
      }
      name
    }
  }
}
  """

  def author: Document = doc"""
{
  author(id: "abc") {
    sys {
      id
      publishedAt
      firstPublishedAt
      publishedVersion
    }
    bio
    image {
      sys {
        id
        publishedAt
        firstPublishedAt
        publishedVersion
      }
      description
      height
      title
      url
      width
    }
    name
  }
}
  """
