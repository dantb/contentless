package io.dantb.contentless.codecs

import cats.data.NonEmptyList
import cats.kernel.Order
import cats.syntax.all.*
import io.circe.{Decoder, Json, JsonObject, Printer}
import io.circe.literal.*
import io.circe.syntax.*
import io.dantb.contentless.{ContentTypeId, Field, Validation}
import io.dantb.contentless.Validation.Regexp
import io.dantb.contentless.codecs.CirceSpec.*
import io.dantb.contentless.codecs.implicits.given
import io.dantb.contentless.dsl.*
import io.dantb.contentless.instances.given
import munit.ScalaCheckSuite
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Prop.*

class CirceSpec extends ScalaCheckSuite:

  given Arbitrary[Json] = Arbitrary(Gen.oneOf(CirceSpec.All))

  property("field serialisation round trip") {
    forAll { (json: Json) =>
      assertEquals(json.as[Field].map(_.asJson.print), Right(json.print))
    }
  }

  extension (j: Json)
    def normalised: Json = j.arrayOrObject[Json](
      j,
      array => Json.fromValues(array.map(_.normalised).sortBy(_.toString)),
      obj =>
        JsonObject.fromIterable {
          obj.toList.map((k, v) => k -> v.normalised).sortBy(_._1)
        }.asJson
    )
    def print: String = j.normalised.printWith(Printer.spaces2SortKeys)

object CirceSpec:

  def All = List(
    LongText,
    ShortText,
    ShortTextDefaulted,
    Boolean,
    Decimal,
    CirceSpec.Json,
    LocalDateTime,
    ZonedDateTime,
    Location,
    TextList,
    Asset,
    MultipleAssets,
    Entry,
    MultipleEntries,
    RichText
  )

  val LongText = json"""
    {
        "id": "longTextField",
        "name": "Long Text Field",
        "type": "Text",
        "localized": false,
        "required": true,
        "validations": [
        {
            "size": {
                "min": 0,
                "max": 50
            },
            "message": "Must be between 0 and 50 characters"
        },
        {
            "regexp": {
                "pattern": "^\\w[\\w.-]*@([\\w-]+\\.)+[\\w-]+$$"
            },
            "message": "Must match email regex case insensitively"
        }
        ],
        "defaultValue": {
            "en-GB": "donald.duck@gmail.com"
        },
        "disabled": false
    }
    """

  val ShortText = json"""
    {
        "id": "shortTextField",
        "name": "Short Text Field Unique URL",
        "type": "Symbol",
        "localized": false,
        "required": false,
        "validations": [
            {
                "unique": true
            },
            {
                "size": {
                    "min": 0,
                    "max": 20
                },
                "message": "Must be between 0 and 20 characters"
            },
            {
                "regexp": {
                    "pattern": "^(ftp|http|https):\/\/(\\w+:{0,1}\\w*@)?(\\S+)(:[0-9]+)?(\/|\/([\\w#!:.?+=&%@!\\-/]))?$"
                },
                "message": "Must be a valid URL"
            },
            {
                "in": [
                    "https://www.itv.com",
                    "https://itv.com/news",
                    "https://papertoilet.com/"
                ]
            }
        ],
        "disabled": false
    }
    """
  val ShortTextDefaulted = json"""
        {
            "id": "shortTextFieldDefaulted",
            "name": "Short Text Field Defaulted",
            "type": "Symbol",
            "localized": false,
            "required": true,
            "validations": [],
            "defaultValue": {
                "en-GB": "Eggy"
            },
            "disabled": false
        }
    """

  val Boolean = json"""
        {
            "id": "booleanField",
            "name": "Boolean Field",
            "type": "Boolean",
            "localized": false,
            "required": true,
            "validations": [],
            "defaultValue": {
                "en-GB": true
            },
            "disabled": false
        }
    """

  val Integer = json"""
        {
            "id": "integerField",
            "name": "Integer Field",
            "type": "Integer",
            "localized": false,
            "required": false,
            "validations": [
                {
                    "range": {
                        "min": -20,
                        "max": 100
                    },
                    "message": "Range: [-20, 100]"
                },
                {
                    "in": [
                        -20,
                        -10,
                        0,
                        50
                    ],
                    "message": "Should be -20, -10, 0 or 50"
                }
            ],
            "defaultValue": {
                "en-GB": -10
            },
            "disabled": false
        }
    """

  val Decimal = json"""
        {
            "id": "decimalField",
            "name": "Decimal Field",
            "type": "Number",
            "localized": false,
            "required": true,
            "validations": [
                {
                    "in": [
                        1.1,
                        2.2,
                        3.3,
                        4.4
                    ]
                }
            ],
            "defaultValue": {
                "en-GB": 2.2
            },
            "disabled": false
        }
    """

  val Json = json"""
        {
            "id": "jsonField",
            "name": "Json Field",
            "type": "Object",
            "localized": false,
            "required": false,
            "validations": [
                {
                    "size": {
                        "min": 1,
                        "max": 10
                    },
                    "message": null
                }
            ],
            "disabled": false
        }
    """

  val LocalDateTime = json"""
        {
            "id": "localDateTimeField",
            "name": "Local Date Time Field",
            "type": "Date",
            "localized": false,
            "required": false,
            "validations": [
                {
                    "dateRange": {
                        "min": null,
                        "max": "2023-05-26T00:00:00Z"
                    }
                }
            ],
            "defaultValue": {
                "en-GB": "2023-05-20T00:00+01:00"
            },
            "disabled": false
        }
    """

  val ZonedDateTime = json"""
        {
            "id": "zonedDateTimeField",
            "name": "Zoned Date Time Field",
            "type": "Date",
            "localized": false,
            "required": true,
            "validations": [
                {
                    "dateRange": {
                        "min": null,
                        "max": "2023-06-21T00:00:00Z"
                    }
                }
            ],
            "defaultValue": {
                "en-GB": "2023-05-30T00:00+01:00"
            },
            "disabled": false
        }
    """

  val Location = json"""
        {
            "id": "locationField",
            "name": "Location Field",
            "type": "Location",
            "localized": false,
            "required": true,
            "disabled": false
        }
    """

  val TextList = json"""
        {
            "id": "textListField",
            "name": "Text List Field",
            "type": "Array",
            "localized": false,
            "required": true,
            "validations": [
                {
                    "size": {
                        "min": 0,
                        "max": 5
                    },
                    "message": "Must have 0-5 symbols"
                }
            ],
            "disabled": false,
            "items": {
                "type": "Symbol",
                "validations": [
                    {
                        "size": {
                            "min": 0,
                            "max": 10
                        },
                        "message": "Must be between 0 and 10 characters"
                    },
                    {
                        "regexp": {
                            "pattern": "[a-zA-Z]"
                        },
                        "message": "Should be some letters"
                    },
                    {
                        "in": [
                            "banana",
                            "bread",
                            "egg",
                            "beans"
                        ]
                    }
                ]
            }
        }
    """

  val Asset = json"""
        {
            "id": "assetField",
            "name": "Asset Field",
            "type": "Link",
            "localized": false,
            "required": true,
            "validations": [
                {
                    "linkMimetypeGroup": [
                        "attachment",
                        "plaintext"
                    ]
                }
            ],
            "disabled": false,
            "linkType": "Asset"
        }
    """

  val MultipleAssets = json"""
        {
            "id": "multipleAssetsField",
            "name": "Multiple Assets Field",
            "type": "Array",
            "localized": false,
            "required": true,
            "validations": [
                {
                "size": {
                    "min": 0,
                    "max": 6
                },
                "message": "0-6 assets pls"
                }
            ],
            "disabled": false,
            "items": {
                "type": "Link",
                "validations": [
                {
                    "linkMimetypeGroup": [
                    "image",
                    "audio"
                    ]
                }
                ],
                "linkType": "Asset"
            }
        }
    """

  val Entry = json"""
        {
            "id": "entryReferenceField",
            "name": "Entry Reference Field",
            "type": "Link",
            "localized": false,
            "required": true,
            "validations": [
                {
                    "linkContentType": [
                        "image",
                        "quotation"
                    ]
                }
            ],
            "disabled": false,
            "linkType": "Entry"
        }
    """

  val MultipleEntries = json"""
        {
            "id": "multipleEntryReferencesField",
            "name": "Multiple Entry References Field",
            "type": "Array",
            "localized": false,
            "required": true,
            "validations": [
                {
                    "size": {
                        "min": 0,
                        "max": 11
                    },
                    "message": "0-11 pls"
                }
            ],
            "disabled": false,
            "items": {
                "type": "Link",
                "validations": [
                    {
                        "linkContentType": [
                            "collection",
                            "accordion"
                        ]
                    }
                ],
                "linkType": "Entry"
            }
        }
    """

  val RichText = json"""
        {
            "id": "richTextField",
            "name": "Rich Text Field",
            "type": "RichText",
            "localized": false,
            "required": true,
            "validations": [
                {
                    "enabledMarks": [
                        "bold",
                        "italic",
                        "underline",
                        "code",
                        "superscript",
                        "subscript"
                    ],
                    "message": "Only underline, subscript, italic, bold, superscript and code marks are allowed"
                },
                {
                    "enabledNodeTypes": [
                        "heading-1",
                        "heading-2",
                        "heading-3",
                        "heading-4",
                        "heading-5",
                        "heading-6",
                        "ordered-list",
                        "unordered-list",
                        "hr",
                        "blockquote",
                        "embedded-entry-block",
                        "embedded-asset-block",
                        "table",
                        "hyperlink",
                        "entry-hyperlink",
                        "asset-hyperlink",
                        "embedded-entry-inline"
                    ],
                    "message": "Only asset hyperlink, embedded asset block, embedded entry block, heading 5, heading 6, hyperlink, heading 3, heading 2, table, hr, heading 4, unordered list, ordered list, blockquote, entry hyperlink, heading 1 and embedded entry inline nodes are allowed"
                },
                {
                    "nodes": {
                        "asset-hyperlink": [
                            {
                                "size": {
                                    "min": 4,
                                    "max": 5
                                },
                                "message": "Must be between 4 and 5 asset links"
                            }
                        ],
                        "embedded-asset-block": [
                            {
                                "size": {
                                    "min": 3,
                                    "max": 4
                                },
                                "message": "Must be between 3 and 4 assets"
                            }
                        ],
                        "embedded-entry-block": [
                            {
                                "linkContentType": [
                                    "allFieldTypes",
                                    "embed"
                                ],
                                "message": "Invalid block entry type"
                            },
                            {
                                "size": {
                                    "min": 2,
                                    "max": 3
                                },
                                "message": "Must be between 2 and 3 entries"
                            }
                        ],
                        "embedded-entry-inline": [
                            {
                                "linkContentType": [
                                    "allFieldTypes",
                                    "collection"
                                ],
                                "message": "Invalid inline entry type"
                            },
                            {
                                "size": {
                                    "min": 5,
                                    "max": 6
                                },
                                "message": "Must be between 5 and 6 inline entries"
                            }
                        ],
                        "entry-hyperlink": [
                            {
                                "linkContentType": [
                                    "allFieldTypes",
                                    "article"
                                ],
                                "message": "Invalid linked entry type"
                            },
                            {
                                "size": {
                                    "min": 1,
                                    "max": 5
                                },
                                "message": "Must be between 1 and 5 entry links"
                            }
                        ]
                    }
                }
            ],
            "disabled": false
        }
    """
