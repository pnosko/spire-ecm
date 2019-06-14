package io.strangeattractor.ecm

import io.strangeattractor.ecm.Alias._
import spire.math.SafeLong

/**
  * Created by Peter on 7/4/2017.
  */
object Curve {

  trait EllipticPoint
  case class AffinePoint(x: Num, y: Num, z: Num) extends EllipticPoint


  object EllipticPoint {
    val affineInfinity: AffinePoint = AffinePoint(SafeLong.zero, SafeLong.zero, SafeLong.zero)
  }

  trait EllipticArithmetic[P <: EllipticPoint] {
    val infinity: P
    val initialPoint: P

    def neg(p: P): P
    def double(p: P): P
    def add(p1: P, p2: P): P
    def mul(p: P, mul: Num): P
  }

  trait EllipticCurve[P <: EllipticPoint] {
    val characteristic: Num
  }
}
