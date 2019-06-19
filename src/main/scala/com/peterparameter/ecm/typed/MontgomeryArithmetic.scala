package com.peterparameter.ecm.typed

import com.peterparameter.ecm.common.Alias.Num
import com.peterparameter.ecm.common.CurveUtils._
import com.peterparameter.ecm.common.Utils._
import com.peterparameter.ecm.typed.ECM.MontgomeryCurve
import spire.math.SafeLong

class MontgomeryArithmetic[N <: Nat : ToSafeLong](curve: MontgomeryCurve[N], val initialPoint: MontgomeryPoint[N]) {
  import MontgomeryPoint._

  val infinity: MontgomeryPoint[N] = MontgomeryPoint[N](SafeLong.zero, SafeLong.zero)
  private val n = ToSafeLong.safeLongN[N]

  def neg(p: MontgomeryPoint[N]): MontgomeryPoint[N] = MontgomeryPoint[N](-p.x, p.z)

  def double(p: MontgomeryPoint[N]): MontgomeryPoint[N] = {
    val xx = p.x * p.x
    val zz = p.z * p.z
    val xz = p.x * p.z
    val diff = (xx - zz + n) % n
    val x =  (diff * diff) % n
    val y =  (four * xz * (xx + curve.a * xz + zz)) % n
    MontgomeryPoint(x, y)
  }

  def add(p1: MontgomeryPoint[N], p2: MontgomeryPoint[N])(origin: MontgomeryPoint[N]): MontgomeryPoint[N] = {
    val d1 = (p1.x * p2.x - p1.z * p2.z) % n
    val d2 = (p1.x * p2.z - p1.z * p2.x) % n
    val x = (origin.z * d1 * d1) % n
    val z = (origin.x * d2 * d2) % n
    MontgomeryPoint(x, z)
  }

  def mul(p: MontgomeryPoint[N], multiple: Num): MontgomeryPoint[N] = multiple match {
    case SafeLong.zero => infinity
    case SafeLong.one => p
    case SafeLong.two => double(p)
    case _ => multiplicationLadder(p, multiple)
  }

  private def multiplicationLadder(p: MontgomeryPoint[N], multiple: Num): MontgomeryPoint[N] = {
    implicit val origin = p
    var u = p
    var t = double(p)

    val bv = multiple.toBitVector
    for (i <- 1 until bv.length) {
      if (bv(i)) {
        u = t + u
        t = t.double
      } else {
        t = u + t
        u = u.double
      }
    }
    u
  }
}