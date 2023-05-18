package io.dantb.contentless.codecs

import cats.data.NonEmptyList
import io.circe.{Decoder, Json, Printer}
import io.circe.literal.*
import io.circe.syntax.*
import io.dantb.contentless.{ContentTypeId, Field, Validation}
import io.dantb.contentless.Validation.Regexp
import io.dantb.contentless.codecs.implicits.given
import io.dantb.contentless.dsl.*
import io.dantb.contentless.instances.given
import munit.ScalaCheckSuite
import org.scalacheck.Prop.*

class CirceSpec extends ScalaCheckSuite:

  test("long text field serialisation round trip") {
    val field = longText(
      "longTextField",
      "Long Text Field",
      defaultValue = Some("donald.duck@gmail.com"),
      charBounds = Some(Validation.Size(Some(0), Some(50), Some("Must be between 0 and 50 characters"))),
      matchesRegex = Some(Validation.RegexpValidation(Regexp.Email, Some("Must match email regex case insensitively")))
    ).required

    val j = json"""
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

    val written: Json               = field.schema.head.asJson
    val read: Decoder.Result[Field] = j.as[Field]

    assertEquals(written.print, j.print)
    assertEquals(read, Right(field.schema.head))
  }

  extension (j: Json) def print: String = j.printWith(Printer.spaces2SortKeys)

  def fullResponse: Json = json"""
{
    "sys": {
        "space": {
            "sys": {
                "type": "Link",
                "linkType": "Space",
                "id": "9wt1zvqpsy8o"
            }
        },
        "id": "allFieldTypes",
        "type": "ContentType",
        "createdAt": "2023-05-18T12:22:18.599Z",
        "updatedAt": "2023-05-18T13:20:01.560Z",
        "environment": {
            "sys": {
                "id": "master",
                "type": "Link",
                "linkType": "Environment"
            }
        },
        "publishedVersion": 33,
        "publishedAt": "2023-05-18T13:20:01.560Z",
        "firstPublishedAt": "2023-05-18T12:22:19.005Z",
        "createdBy": {
            "sys": {
                "type": "Link",
                "linkType": "User",
                "id": "6gaSKdIG0FToScPLiTwecW"
            }
        },
        "updatedBy": {
            "sys": {
                "type": "Link",
                "linkType": "User",
                "id": "6gaSKdIG0FToScPLiTwecW"
            }
        },
        "publishedCounter": 17,
        "version": 34,
        "publishedBy": {
            "sys": {
                "type": "Link",
                "linkType": "User",
                "id": "6gaSKdIG0FToScPLiTwecW"
            }
        }
    },
    "displayField": "longTextField",
    "name": "All Field Types",
    "description": "This content type has every field type with validations enabled (minus all permutations). Used to generated an API response for round trip Scala DSL tests.",
    "fields": [
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
                        "pattern": "^\\w[\\w.-]*@([\\w-]+\\.)+[\\w-]+$",
                        "flags": "i"
                    },
                    "message": "Must match email regex case insensitively"
                },
                {
                    "prohibitRegexp": {
                        "pattern": "mickey.mouse@hotmail.com",
                        "flags": "i"
                    },
                    "message": "Mickey mouse isn't allowed"
                }
            ],
            "defaultValue": {
                "en-GB": "donald.duck@gmail.com"
            },
            "disabled": false,
            "omitted": false
        },
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
                        "pattern": "^(ftp|http|https):\/\/(\\w+:{0,1}\\w*@)?(\\S+)(:[0-9]+)?(\/|\/([\\w#!:.?+=&%@!\\-/]))?$",
                        "flags": null
                    },
                    "message": "Must be a valid URL"
                },
                {
                    "in": [
                        "https://www.itv.com",
                        "https://itv.com/news",
                        "https://papertoilet.com/"
                    ],
                    "message": "Must match predefined URLs"
                }
            ],
            "disabled": false,
            "omitted": false
        },
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
            "disabled": false,
            "omitted": false
        },
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
            "disabled": false,
            "omitted": false
        },
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
            "disabled": false,
            "omitted": false
        },
        {
            "id": "decimalField",
            "name": "Decimal Field",
            "type": "Number",
            "localized": false,
            "required": true,
            "validations": [
                {
                    "range": {
                        "min": 0,
                        "max": 5
                    },
                    "message": "Range: [0, 5]"
                },
                {
                    "in": [
                        1.1,
                        2.2,
                        3.3,
                        4.4
                    ],
                    "message": "Invalid decimal value"
                }
            ],
            "defaultValue": {
                "en-GB": 2.2
            },
            "disabled": false,
            "omitted": false
        },
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
                    "message": "JSON must have between 1 and 10 properties"
                }
            ],
            "disabled": false,
            "omitted": false
        },
        {
            "id": "localDateTimeField",
            "name": "Local Date Time Field",
            "type": "Date",
            "localized": false,
            "required": false,
            "validations": [
                {
                    "dateRange": {
                        "after": null,
                        "before": null,
                        "max": "2023-05-26"
                    },
                    "message": "Not in date range"
                }
            ],
            "defaultValue": {
                "en-GB": "2023-05-20T00:00+01:00"
            },
            "disabled": false,
            "omitted": false
        },
        {
            "id": "zonedDateTimeField",
            "name": "Zoned Date Time Field",
            "type": "Date",
            "localized": false,
            "required": true,
            "validations": [
                {
                    "dateRange": {
                        "after": null,
                        "before": null,
                        "max": "2023-06-21"
                    },
                    "message": "Must be in range"
                }
            ],
            "defaultValue": {
                "en-GB": "2023-05-30T00:00+01:00"
            },
            "disabled": false,
            "omitted": false
        },
        {
            "id": "locationField",
            "name": "Location Field",
            "type": "Location",
            "localized": false,
            "required": true,
            "validations": [],
            "disabled": false,
            "omitted": false
        },
        {
            "id": "richTextField",
            "name": "Rich Text Field",
            "type": "RichText",
            "localized": false,
            "required": true,
            "validations": [
                {
                    "size": {
                        "min": 0,
                        "max": 1000
                    },
                    "message": "Must be between 0 and 1000 characters"
                },
                {
                    "enabledMarks": [
                        "bold",
                        "italic",
                        "underline",
                        "code",
                        "superscript",
                        "subscript"
                    ],
                    "message": "Only bold, italic, underline, code, superscript, and subscript marks are allowed"
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
                    "message": "Only heading 1, heading 2, heading 3, heading 4, heading 5, heading 6, ordered list, unordered list, horizontal rule, quote, block entry, asset, table, link to Url, link to entry, link to asset, and inline entry nodes are allowed"
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
            "disabled": false,
            "omitted": false
        },
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
            "omitted": false,
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
                            "pattern": "[a-zA-Z]",
                            "flags": "g"
                        },
                        "message": "Should be some letters"
                    },
                    {
                        "in": [
                            "banana",
                            "bread",
                            "egg",
                            "beans"
                        ],
                        "message": "Must be banana bread egg beans"
                    }
                ]
            }
        },
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
                    ],
                    "message": "Only attachment or plaintext allowed"
                },
                {
                    "assetImageDimensions": {
                        "width": {
                            "min": 6,
                            "max": null
                        },
                        "height": {
                            "min": 501,
                            "max": null
                        }
                    },
                    "message": "Must be at least 600x500"
                },
                {
                    "assetFileSize": {
                        "min": 0,
                        "max": 104857600
                    },
                    "message": "Must be 0-100MB"
                }
            ],
            "disabled": false,
            "omitted": false,
            "linkType": "Asset"
        },
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
            "omitted": false,
            "items": {
                "type": "Link",
                "validations": [
                    {
                        "linkMimetypeGroup": [
                            "image",
                            "audio"
                        ],
                        "message": "image or audio pls"
                    }
                ],
                "linkType": "Asset"
            }
        },
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
                    ],
                    "message": "Invalid content type for entry"
                }
            ],
            "disabled": false,
            "omitted": false,
            "linkType": "Entry"
        },
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
            "omitted": false,
            "items": {
                "type": "Link",
                "validations": [
                    {
                        "linkContentType": [
                            "collection",
                            "accordion"
                        ],
                        "message": "Invalid entry type"
                    }
                ],
                "linkType": "Entry"
            }
        }
    ]
}
  """
