package io.dantb.contentless

import cats.Eq
import io.dantb.contentless.webhook.WebhookDefinition

sealed trait Versioned[A] {
  def version: Lens[A, Option[Int]]
}

object Versioned {

  implicit def entryVersion[A]: Versioned[Entry[A]] =
    new Versioned[Entry[A]] {
      override def version: Lens[Entry[A], Option[Int]] =
        new Lens[Entry[A], Option[Int]] {
          override def get(s: Entry[A]): Option[Int] = s.version

          override def set(s: Entry[A])(a: Option[Int]): Entry[A] = s.copy(version = a)
        }
    }

  implicit val webhookVersion: Versioned[WebhookDefinition] =
    new Versioned[WebhookDefinition] {
      override def version: Lens[WebhookDefinition, Option[Int]] =
        new Lens[WebhookDefinition, Option[Int]] {
          override def get(s: WebhookDefinition): Option[Int] = s.version

          override def set(s: WebhookDefinition)(a: Option[Int]): WebhookDefinition = s.copy(version = a)
        }
    }

  implicit val contentTypeVersion: Versioned[ContentType] =
    new Versioned[ContentType] {
      override def version: Lens[ContentType, Option[Int]] =
        new Lens[ContentType, Option[Int]] {
          override def get(s: ContentType): Option[Int] = s.version

          override def set(s: ContentType)(a: Option[Int]): ContentType = s.copy(version = a)
        }
    }

  def apply[A](implicit versioned: Versioned[A]): Versioned[A] = versioned

  def unversioned[A: Versioned](a: A): A = Versioned[A].version.set(a)(None)

  def eqUnversionedUniversalEquals[A: Versioned]: Eq[A] = Eq.instance((a, b) => unversioned(a) == unversioned(b))

}
