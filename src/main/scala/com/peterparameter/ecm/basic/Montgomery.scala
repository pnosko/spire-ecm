package com.peterparameter.ecm.basic

import com.peterparameter.ecm.common.Alias.Num
import com.peterparameter.ecm.common.Utils
import spire.math._
import spire.random._

import scala.util.Try

/**
  */
object Montgomery {
  import SafeLong._
  import Utils._

  def arithmetic(curve: MontgomeryCurve, initialPoint: MontgomeryPoint): MontgomeryArithmetic =
    new MontgomeryArithmetic(curve, initialPoint)

  case class Factor(n: Num)

  /*
  * Projective point on the elliptic curve, with Y-coordinate omitted (x = X / (Z ^ 2), y = X / (Z ^ 3))
  */
  case class MontgomeryPoint(x: Num, z: Num)

  /*
  * Curve B * y ^ 2 = x ^ 3 + A * x ^ 2 + x
  */
  case class MontgomeryCurve(a: Num, b: Num, characteristic: Num)

  def generate(n: Num, rng: Generator = spire.random.GlobalRng): CurveResult =
    genCurve(n, getSigma(n, rng))

  private def getSigma(n: Num, rng: Generator): Num = {
    val byteLength = max(1, n.bitLength / 8)
    val dist = Dist.bigint(byteLength)

    val start = SafeLong(7)
    val random = rng.next[BigInt](dist).toSafeLong % (n - start)
    val sigma = start + random
    sigma
  }

  type CurveResult = Either[Factor, (MontgomeryCurve, MontgomeryPoint)]
  private def genCurve(n: Num, sigma: Num): CurveResult = {
    def modInv(number: Num): Option[Num] = Try(SafeLong(number.toBigInt.modInverse(n.toBigInt))).toOption

    val four = SafeLong(4)

    val u = sigma * sigma - SafeLong(5)
    val v = four * sigma

    val x = u * u * u % n
    val z = v * v * v % n

    val candidate = four * x * v % n

    def degenerate(candidate: Num): CurveResult = {
      val gcd = candidate.gcd(n)
      if (gcd === n) generate(n)
      else Left(Factor(gcd))
    }

    def nonDegenerate(t1: Num): CurveResult = {
      val t2 = (v - u + n) % n
      val a = (t2 * t2 * t2 * (three * u + v) * t1 - two) % n
      Right((MontgomeryCurve(a, one, n), MontgomeryPoint(x % n, z % n)))
    }

    val inverse = modInv(candidate)

    inverse.fold(degenerate(candidate))(nonDegenerate)
  }

  class MontgomeryArithmetic(curve: MontgomeryCurve, val initialPoint: MontgomeryPoint) {
    import Utils._
    private val four = SafeLong(4L)

    val infinity: MontgomeryPoint = MontgomeryPoint(SafeLong.zero, SafeLong.zero)
    private val n = curve.characteristic

    def neg(p: MontgomeryPoint): MontgomeryPoint = MontgomeryPoint(-p.x, p.z)

    def double(p: MontgomeryPoint): MontgomeryPoint = {
      val xx = p.x * p.x
      val zz = p.z * p.z
      val xz = p.x * p.z
      val diff = (xx - zz + n) % n
      val x =  (diff * diff) % n
      val y =  (four * xz * (xx + curve.a * xz + zz)) % n
      MontgomeryPoint(x, y)
    }

    def add(p1: MontgomeryPoint, p2: MontgomeryPoint)(origin: MontgomeryPoint): MontgomeryPoint = {
      val d1 = (p1.x * p2.x - p1.z * p2.z) % n
      val d2 = (p1.x * p2.z - p1.z * p2.x) % n
      val x = (origin.z * d1 * d1) % n
      val z = (origin.x * d2 * d2) % n
      MontgomeryPoint(x, z)
    }

    private def multiplicationLadder(p: MontgomeryPoint, multiple: Num): MontgomeryPoint = {
      var u: MontgomeryPoint = p
      var t: MontgomeryPoint = double(p)

      val bv = multiple.toBitVector
      val range = 1 until bv.length
      for (i <- range) {
        if (bv(i)) {
          u = add(t, u)(p)
          t = double(t)
        } else {
          t = add(u, t)(p)
          u = double(u)
        }
      }
      u
    }

    def mul(p: MontgomeryPoint, multiple: Num): MontgomeryPoint = multiple match {
      case SafeLong.zero => infinity
      case SafeLong.one => p
      case SafeLong.two => double(p)
      case _ => multiplicationLadder(p, multiple)
    }
  }
}

