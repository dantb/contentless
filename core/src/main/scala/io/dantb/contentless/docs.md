
# Changes from current user-facing API
1. Always use "Pretty" codecs. It's better to default these explicitly than leaving up to Contentful. Removing the pretty prefix altogether.
2. Twiddle lists for composition instead of "imap".
3. In order to do anything with content types, such as create / update / fetch content types, or instances of content types (entries), we need both field definitions and content type information such as name, id, displayName. Therefore we only expose one abstraction - `ContentType`.
4. Separate DSL, codecs, and internal types more explicitly. Users should only interact with the DSL. Internal types are purely for the library's interactions with Contentful's APIs. Codecs join the two together.
5. Errors now extend `Throwable`. Raising errors through `MonadThrow` is now the default.
