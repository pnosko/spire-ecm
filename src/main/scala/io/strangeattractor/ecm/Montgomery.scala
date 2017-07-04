package io.strangeattractor.ecm

import io.strangeattractor.ecm.Alias.Num
import io.strangeattractor.ecm.Curve.{EllipticCurve, EllipticPoint}
import spire.math.SafeLong

/**
  * Created by Peter on 7/4/2017.
  */
object Montgomery {
  case class MontgomeryPoint(x: Num, z: Num) extends EllipticPoint
  val montgomeryInfinity: MontgomeryPoint = MontgomeryPoint(SafeLong.zero, SafeLong.zero)

  class MontgomeryGenerator extends CurveGenerator[MontgomeryPoint] {
    def generate(n: Num): (MontgomeryPoint, MontgomeryCurve) = {

    }
  }

  class MontgomeryCurve(val characteristic: Num) extends EllipticCurve[MontgomeryPoint] {

    override val infinity: MontgomeryPoint = montgomeryInfinity

    override def neg(p: MontgomeryPoint): MontgomeryPoint = ???

    override def double(p: MontgomeryPoint): MontgomeryPoint = ???

    override def add(p1: MontgomeryPoint, p2: MontgomeryPoint): MontgomeryPoint = ???

    override def mul(p: MontgomeryPoint, mul: Num): MontgomeryPoint =
      if (mul.isZero) infinity
      else {
        infinity
      }
  }
}
