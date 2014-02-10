package io.strangeattractor.ecm.tryout

import shapeless._
import shapeless.ops.nat._
import spire.algebra._
import spire.math._
import spire.syntax.integral._

final class Z[N <: Nat] private (val n: Int) /*extends AnyVal*/ {

  // Remove if extending AnyVal.
  override def equals(obj: Any) = obj match {
    case that: Z[_] => that.n == n
    case _ => false
  }

  override def toString = "Z(%d)" format n
}


object Z {
  def unapply(n: Z[_]) = Some(n.n)

  def apply[A <: Nat](n: Int)(implicit toInt: ToInt[A]): Z[A] = {
    val k = (if (n < 0) -n else n) % toInt()
    new Z[A](k)
  }

  implicit def eq[N <: Nat] = new Eq[Z[N]] {
    def eqv(a: Z[N], b: Z[N]) = a.n == b.n
  }

  implicit def ring[N <: Nat](implicit ord: ToInt[N]): Ring[Z[N]] = new FiniteRing[N] {
    val toInt: ToInt[N] = ord
  }
}