package io.strangeattractor.ecm

import io.strangeattractor.ecm.Alias.Num
import io.strangeattractor.ecm.Curve._
import cats.data._
import spire.algebra.Group
import spire.math.{SafeLong, _}
import spire.random._

import scala.collection.immutable.NumericRange
import scala.util.Try

/**
  */
object Montgomery {
  import Utils._

  /*
  * Point on the elliptic curve, with Y-coordinate omitted
  */
  case class MontgomeryPoint(x: Num, z: Num) extends EllipticPoint

  /*
  * Curve y ^ 2 = x ^ 3 + A * x ^ 2 + x
  */
  case class MontgomeryCurve(a: Num, characteristic: Num) extends EllipticCurve[MontgomeryPoint]

  class MontgomeryGenerator(rng: Generator = spire.random.GlobalRng) {

    def generate(n: Num): Either[Factor, (MontgomeryCurve, MontgomeryPoint)] = genCurve(n, getSigma(n))

    private def getSigma(n: Num) = {
      val byteLength = max(1, n.bitLength / 8)
      val dist = Dist.bigint(byteLength)

      val start = SafeLong(7)
      val random = rng.next[BigInt](dist).toSafeLong % (n - start)
      val sigma = start + random
      sigma
    }

    private def genCurve(n: Num, sigma: Num): Either[Factor, (MontgomeryCurve, MontgomeryPoint)] = {
      def modInv(number: Num): Option[Num] = Try(SafeLong(number.toBigInt.modInverse(n.toBigInt))).toOption

      val four = SafeLong(4)

      val u = sigma * sigma - SafeLong(5)
      val v = four * sigma

      val x = u * u * u % n
      val z = v * v * v % n

      val candidate = four * x * v % n

      def degenerate(candidate: Num) = {
        val gcd = candidate.gcd(n)
        if (gcd === n) generate(n)
        else Left(Factor(gcd))
      }

      def nonDegenerate(t1: Num) = {
        val t2 = (v - u + n) % n
        val a = (t2 * t2 * t2 * (SafeLong.three * u + v) * t1 - SafeLong.two) % n
        Right(MontgomeryCurve(a, n), MontgomeryPoint(x % n, z % n))
      }

      val inverse = modInv(candidate)

      inverse.fold(degenerate(candidate))(nonDegenerate)
    }
  }

  class MontgomeryArithmetic(curve: MontgomeryCurve, val initialPoint: MontgomeryPoint) {
    import Utils._

    val infinity: MontgomeryPoint = MontgomeryPoint(SafeLong.zero, SafeLong.zero)
    private val n = curve.characteristic

    def neg(p: MontgomeryPoint): MontgomeryPoint = MontgomeryPoint(-p.x, p.z)

    def double(p: MontgomeryPoint): MontgomeryPoint = {
      val xx = p.x * p.x
      val zz = p.z * p.z
      val xz = p.x * p.z
      val diff = (xx - zz + n) % n
      val x =  (diff * diff) % n
      val y =  (SafeLong(4L) * xz * (xx + curve.a * xz + zz)) % n
      MontgomeryPoint(x, y)
    }

    def add(p1: MontgomeryPoint, p2: MontgomeryPoint, p3: MontgomeryPoint): MontgomeryPoint = {
      val d1 = (p1.x * p2.x - p1.z * p2.z) % n
      val d2 = (p1.x * p2.z - p1.z * p2.x) % n
      val x = (p3.z * d1 * d1) % n
      val z = (p3.x * d2 * d2) % n
      MontgomeryPoint(x, z)
    }

    private def multiplicationLadder(p: MontgomeryPoint, multiple: Num) = {
      var u: MontgomeryPoint = p
      var t: MontgomeryPoint = double(p)

      val bv = multiple.toBitVector
      for (i <- (bv.length - 2) to 0 by -1) {
        if (bv(i)) {
          u = add(t, u, p)
          t = double(t)
        } else {
          t = add(u, t, p)
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

