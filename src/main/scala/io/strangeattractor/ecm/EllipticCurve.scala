//package io.strangeattractor.ecm
//
////import scalaz._
////import Scalaz._
//import spire.algebra._
//import spire.math._
//import shapeless.Nat
//import io.strangeattractor.ecm.tryout.FiniteRing
//import shapeless.ops.nat.ToInt
//
//class Characteristic[A: Integral](val characteristic: A) {
//  type NumberType = A
//  case class Order(order: A)
//}
//
//trait Zn[A] {
//  val order: A
//}
//
//final class Z[A : Integral] private (val characteristic: A, val value: A) extends Zn[A] {
//  val order = characteristic
//
//  val n = abs(value) % characteristic
//}
//
//object Z {
//  def unapply(n: Z[_]) = Some(n.n)
//
//  def apply[A : Integral](order: A, n: A): Z[A] = new Z[A](order, n)
//
//  implicit def eq[A: Integral] = new Eq[Z[A]] {
//    def eqv(a: Z[A], b: Z[A]) = a.order == b.order && a.n == b.n
//  }
//
//  implicit def ring[N <: Nat]: Ring[Z[N]] = new FiniteRing[N] {
//    val toInt: ToInt[N] = ord
//  }
//}
//
//trait EllipticPseudoCurve[A <: Z[_]] extends Ring[A] {
//
//}
//
//trait EllipticCurve[A <: Z[_]] extends Field[A] {
//
//}
//
//trait Point[R <: Ring[_]] {
//
//}