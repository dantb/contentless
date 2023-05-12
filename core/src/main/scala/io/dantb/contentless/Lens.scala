package io.dantb.contentless

trait Lens[S, A] {
  def get(s: S): A
  def set(s: S)(a: A): S
}
