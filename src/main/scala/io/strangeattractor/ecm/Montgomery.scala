package io.strangeattractor.ecm

import io.strangeattractor.ecm.Alias.Num
import io.strangeattractor.ecm.Curve._
import cats.data._
import spire.algebra.Group
import spire.math._
import spire.random._

import scala.util.Try

/**
  */
object Montgomery {
  trait MontgomeryMethod


  case class MontgomeryPoint(x: Num, z: Num) extends EllipticPoint
  val montgomeryInfinity: MontgomeryPoint = MontgomeryPoint(SafeLong.zero, SafeLong.zero)

  class MontgomeryGenerator(rng: Generator = spire.random.GlobalRng) {

    private def genCurve(n: Num): Either[Factor, (MontgomeryCurve, MontgomeryPoint)] = {
      def degenerate(candidate: Num, n: Num) = {
        val gcd = candidate.gcd(n)
        if (gcd === n) genCurve(n)
        else Left(Factor(gcd))
      }

      val zero = SafeLong.zero; val two = SafeLong.two; val four = SafeLong(4); val five = SafeLong(5)
      val start = 7
      val sigma = SafeLong(start + rng.nextInt(Int.MaxValue - start))
      val u = sigma * sigma - five

      val v = four * sigma
      val x = u * u * u

      val z = v * v * v

      val candidate = SafeLong(4) * x * v

      val g = MulGroup.gen(n)         // TODO: memoize the group?
      val inverse = Try(g.inverse(candidate))

      inverse.fold (_ => degenerate(candidate, n),
        t1 => {
          val t2 = v - u
          val a = t2 * t2 * t2 * (SafeLong.three * u + v) * t1 - two
          Right(MontgomeryCurve(a, zero, n), MontgomeryPoint(x % n, z % n))
        }
      )
    }

    def generate(n: Num): Either[Factor, (MontgomeryCurve, MontgomeryPoint)] = {
      genCurve(n)
    }
  }

  class MontgomeryArithmetic(curve: MontgomeryCurve, val initialPoint: MontgomeryPoint) extends EllipticArithmetic[MontgomeryPoint] {
    import Utils._

    val infinity: MontgomeryPoint = montgomeryInfinity

    def neg(p: MontgomeryPoint): MontgomeryPoint = MontgomeryPoint(-p.x, p.z)

    override def double(p: MontgomeryPoint): MontgomeryPoint = {
      val xx = p.x * p.x
      val zz = p.z * p.z
      val xz = p.x * p.z
      val diff = xx - zz
      val x =  diff * diff % curve.characteristic
      val y = 4 * xz * (xx + curve.a * xz + zz) % curve.characteristic
      MontgomeryPoint(x, y)
    }

    override def add(p1: MontgomeryPoint, p2: MontgomeryPoint): MontgomeryPoint = {
      val d1 = p1.x * p2.x - p1.z * p2.z
      val d2 = p1.x * p2.z - p1.z * p2.x
      val four = SafeLong(4L)
      val x = four * initialPoint.z * d1 * d1 % curve.characteristic
      val z = four * initialPoint.x * d2 * d2 % curve.characteristic
      //      val left = (p1.x - p1.z) * (p2.x + p2.z)
      //      val right = (p1.x + p1.z) * (p2.x - p2.z)
      //      val x = initialPoint.z * (left + right) ** 2 % characteristic
      //      val z = initialPoint.x * (left - right) ** 2 % characteristic
      MontgomeryPoint(x, z)
    }

    def mul(p: MontgomeryPoint, multiple: Num): MontgomeryPoint = multiple match {
      case SafeLong.zero => infinity
      case SafeLong.one => p
      case SafeLong.two => double(p)
      case _ => {
        var u: MontgomeryPoint = p
        var t: MontgomeryPoint = double(p)

        val bv = multiple.toBitVector
        for (i <- 1L to (bv.length - 1L)) {
          if (bv.get(i)) {
            u = add(t, u)
            t = double(t)
          } else {
            t = add(u, t)
            u = double(u)
          }
        }
        u
      }
    }
  }

  case class MontgomeryCurve(a: Num, b: Num, characteristic: Num) extends EllipticCurve[MontgomeryPoint]
}

object MulGroup {
  def gen(n: Num): Group[Num] = new Group[Num]{
    override def inverse(a: Num): Num = SafeLong(a.toBigInt.modInverse(n.toBigInt))

    override def empty: Num = SafeLong.one

    override def combine(x: Num, y: Num): Num = x * y % n
  }

}
