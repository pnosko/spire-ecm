package io.strangeattractor.ecm.tryout

import spire.algebra._
import spire.math._

case class AffinePoint[A <: Ring[Z[_]]](x: Z[_], y: Z[_]) {
  
//  def +(other: AffinePoint[A])(implicit AffineEllipticCurve[A]): AffinePoint[A] = {
//    
//  }
  
}

object AffinePoint {
  def zero[A <: FiniteRing[_]](r: A) = new AffinePoint[A](r.zero, r.one)
}

class AffineEllipticPseudoCurve[A <: FiniteRing[N], N]
  (val ring: A, val initialPoint: AffinePoint[A]) 
  extends Ring[AffinePoint[A]] {
  
  lazy val zero = AffinePoint.zero(ring)
  lazy val one = initialPoint
  def negate(a: AffinePoint[A])
  def plus(a: AffinePoint[A], b: AffinePoint[A])
  def times(a: AffinePoint[A], b: AffinePoint[A])
}