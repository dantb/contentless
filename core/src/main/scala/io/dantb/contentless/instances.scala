package io.dantb.contentless

import cats.Eq
import cats.data.NonEmptyList
import cats.syntax.all.*
import io.circe.Encoder

protected object instances:

  def eqSet[A: Eq]: Eq[Set[A]] = (as, bs) => as.size == bs.size && as.forall(a => bs.exists(b => b === a))

  given nelEncoder[A: Encoder]: Encoder[NonEmptyList[A]] = Encoder.encodeList[A].contramap(_.toList)
