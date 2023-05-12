package io.dantb.contentless

import cats.Eq
import cats.syntax.all.*

protected object instances {

  def eqSet[A: Eq]: Eq[Set[A]] = (as, bs) => as.size == bs.size && as.forall(a => bs.exists(b => b === a))

}
