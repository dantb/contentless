package io.dantb.contentless

import io.dantb.contentless.Entry.{Authors, Timestamps}

object syntax:
  implicit class ContentModelOps[A](val value: A) extends AnyVal:
    def toEntry(version: Option[Int] = None): Entry[A] =
      Entry(None, value, version, None, None, Timestamps.none, Authors.none)
