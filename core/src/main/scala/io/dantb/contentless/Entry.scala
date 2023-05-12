package io.dantb.contentless

import java.time.ZonedDateTime

import cats.Eq
import cats.kernel.Semigroup
import cats.syntax.all.*
import io.dantb.contentless.Entry.{Authors, Timestamps}

final case class Entry[A](
    id: Option[String],
    fields: A,
    version: Option[Int], // this is only optional on the delivery API.
    space: Option[Reference],
    environment: Option[Reference],
    timestamps: Timestamps,
    authors: Authors
):
  def withVersion(version: Int): Entry[A] = copy(version = version.some)

object Entry:

  final case class Timestamps(
      createdAt: Option[ZonedDateTime],
      updatedAt: Option[ZonedDateTime],
      publishedAt: Option[ZonedDateTime],
      firstPublishedAt: Option[ZonedDateTime]
  )
  object Timestamps:
    def none: Timestamps = Timestamps(None, None, None, None)

  final case class Authors(
      createdBy: Option[Reference],
      updatedBy: Option[Reference],
      publishedBy: Option[Reference]
  )
  object Authors:
    def none = Authors(None, None, None)

  def apply[A](fields: A): Entry[A] = Entry(None, fields, None, None, None, Timestamps.none, Authors.none)

  implicit def semigroup[A: Semigroup]: Semigroup[Entry[A]] =
    Semigroup.instance { (a, b) =>
      a.copy(fields = a.fields.combine(b.fields))
    }

  def fields[A]: Lens[Entry[A], A] = new Lens[Entry[A], A]:
    override def get(s: Entry[A]): A = s.fields

    override def set(s: Entry[A])(a: A): Entry[A] = s.copy(fields = a)

  implicit def eqInstance[A: Eq]: Eq[Entry[A]] = Eq.instance((a, b) => Eq[A].eqv(a.fields, b.fields))
