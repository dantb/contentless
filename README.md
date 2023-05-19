# contentless
Purely functional Scala DSLs for Contentful APIs


## Unsupported 
Contentful has a lot of features which we haven't needed but may be added in future. These are the ones we know about.

### Field validations
There are highly specific field validations and customisation being added to Contentful regularly. These are some we're yet to support. Content Management API details are included where known.

- Symbol / Text / Symbol list:
   - Prohibiting regex strings: `prohibitRegexp`
   - Regex flags, e.g. `"flags": "i"`
   - Custom error message for accepting only specified values
- Integer / Decimal:
   - Custom error message for accepting only specified values
- Decimal:
   - Accept a range min/max and custom error message
- Json:
   - Custom error message for property range
- Asset / Asset list:
   - Image dimensions in pixels: `assetImageDimensions`
   - File size in B/KB/MB: `assetFileSize`
   - Custom error message for allowed mime type groups
- Entry / Entry list
   - Custom error message for allowed content types
- DateTime
   - Custom error message for date range
- RichText
   - Accept a character range min/max and custom error message

### Quirks
- `contentless` normalises date fields to use `ZonedDateTime` where possible. This works around how Contentful allows many different types to be stored in the same field; returned in the API as a string.