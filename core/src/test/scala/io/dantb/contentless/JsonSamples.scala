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

import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.EncoderOps
import io.dantb.contentless.JsonSamples.Optics.*

object JsonSamples {

  object Optics {
    def dropField(field: String): Json => Json   = j => j.asObject.map(_.remove(field).asJson).getOrElse(j)
    def dropSecret: Json => Json                 = dropField("secret")
    def addValue(value: String): Json => Json    = j => j.asObject.map(_.add("value", value.asJson).asJson).getOrElse(j)
    def addSecret(secret: Boolean): Json => Json = j => j.asObject.map(_.add("secret", secret.asJson).asJson).getOrElse(j)
  }

  val webhookHeaderKeySecret: Json =
    json"""
          {
              "key": "Mr",
            "secret": true
          }
          """

  val webhookHeaderKeyVal: Json = (dropSecret andThen addValue("potatohead"))(webhookHeaderKeySecret)

  val locationJson: Json =
    json"""
        {
          "lat": 1.234,
          "lon": 5.678
        }
        """

  val entryJson: Json =
    json"""
        {
          "sys": {
            "space": {
              "sys": {
                "type": "Link",
                "linkType": "Space",
                "id": "ti0n6mm28of1"
              }
            },
            "id": "bxs872k",
            "version": 119,
            "environment": {
              "sys": {
                "id": "master",
                "type": "Link",
                "linkType": "Environment"
              }
            },
            "createdAt": "2019-11-20T11:50:54.417Z",
            "updatedAt": "2019-11-20T11:50:54.417Z",
            "publishedAt": "2019-11-20T11:50:54.417Z",
            "createdBy": {
              "sys": {
                "id": "3o5NNVMId3IVVXGSRFumug",
                "type": "Link",
                "linkType": "User"
              }
            },
            "updatedBy": {
              "sys": {
                "id": "3o5NNVMId3IVVXGSRFumug",
                "type": "Link",
                "linkType": "User"
              }
            }
          },
          "fields": {
            "name": {
              "en-GB": "Dan"
            },
            "age": {
              "en-GB": 100
            },
            "birthday": {
              "en-GB": "2019-11-20T11:50:54.417Z"
            },
            "location": {
              "en-GB": {
                "lat": 1.2345,
                "lon": 6.789
              }
            },
            "family": {
              "en-GB": [
                {
                  "sys": {
                    "type": "Link",
                    "linkType": "Entry",
                    "id": "potato"
                  }
                },
                {
                  "sys": {
                    "type": "Link",
                    "linkType": "Entry",
                    "id": "sprouts"
                  }
                }
              ]
            }
          }
        }
      """

  val richTextNodesJson: Json =
    json"""
                {
                    "nodes": {
                        "embedded-asset-block": [
                            {
                                "size": {
                                    "min": 0,
                                    "max": 10
                                },
                                "message": "Asset embedded msg"
                            }
                        ],
                        "embedded-entry-block": [
                            {
                                "linkContentType": [
                                    "accordion",
                                    "image",
                                    "article"
                                ],
                                "message": "Entry type block message"
                            },
                            {
                                "size": {
                                    "min": 0,
                                    "max": 80
                                },
                                "message": "Embedded block limit"
                            }
                        ],
                        "embedded-entry-inline": [
                            {
                                "linkContentType": [
                                    "automatedShelf",
                                    "nav"
                                ],
                                "message": "inline entry msg"
                            },
                            {
                                "size": {
                                    "min": null,
                                    "max": 10
                                }
                            }
                        ],
                        "entry-hyperlink": [
                            {
                                "linkContentType": [
                                    "quotation",
                                    "region"
                                ],
                                "message": "link to entry message"
                            },
                            {
                                "size": {
                                    "min": 0,
                                    "max": null
                                },
                                "message": null
                            }
                        ]
                    }
                }
        """

}
