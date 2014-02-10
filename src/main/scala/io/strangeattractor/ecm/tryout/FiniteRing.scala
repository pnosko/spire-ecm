package io.strangeattractor.ecm.tryout

import shapeless._
import shapeless.ops.nat._
import spire.algebra._
import spire.math._
import spire.syntax.integral._

trait FiniteRing[N <: Nat] extends Ring[Z[N]] {
  implicit def toInt: ToInt[N]

  def order: Int = toInt()

  lazy val zero = Z[N](0)
  lazy val one = Z[N](1)
  def negate(a: Z[N]) = Z[N](order - a.n)
  def plus(a: Z[N], b: Z[N]) = Z[N](a.n + b.n)
  def times(a: Z[N], b: Z[N]) = Z[N](a.n * b.n)
}

object FiniteRing {

}