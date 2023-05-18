# contentless
Purely functional Scala DSLs for Contentful APIs


## Unsupported 
Contentful has a lot of features which we haven't needed but may be added in future. These are the ones we know about.

### Field validations
There are highly specific field validations being added all the time. These are some we're yet to support. Content Management API details are included where known.

- Symbol / Text / Symbol list :
   - Prohibiting regex strings: `prohibitRegexp`
   - Regex flags, e.g. `"flags": "i"`
- Asset / Asset list:
   - Image dimensions in pixels: `assetImageDimensions`
   - File size in B/KB/MB: `assetFileSize`
