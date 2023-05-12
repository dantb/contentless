package io.dantb.contentless

import io.circe.Json

/** List of a specific type of entry, plus arbitrary entries and assets referenced by those entries.
  */
final case class EntriesAndIncludes[A](
    entries: List[Entry[A]],
    includedEntries: List[Json],
    includedAssets: List[Json],
    skip: Int,
    limit: Int,
    total: Int
)
