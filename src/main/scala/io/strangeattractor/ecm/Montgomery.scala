package io.strangeattractor.ecm

import io.strangeattractor.ecm.Alias.Num
import io.strangeattractor.ecm.Curve._
import io.strangeattractor.ecm.Method._
import spire.algebra.{Group, MultiplicativeGroup}
import spire.math._
import spire.random._

/**
  */
object Montgomery {
  trait MontgomeryMethod extends FactorizationMethod

  implicit val montgomery: FactorizationResolver[MontgomeryMethod] = new FactorizationResolver[MontgomeryMethod] {
    type Point = MontgomeryPoint
    type Curve = MontgomeryCurve
    val generator = new MontgomeryGenerator
  }

  case class MontgomeryPoint(x: Num, z: Num) extends EllipticPoint
  val montgomeryInfinity: MontgomeryPoint = MontgomeryPoint(SafeLong.zero, SafeLong.zero)

  class MontgomeryGenerator extends CurveGenerator[MontgomeryPoint] {
    val rng: Generator = spire.random.GlobalRng

    private def genCurve(n: Num): MontgomeryCurve = {
      val zero = SafeLong.zero; val two = SafeLong.two; val four = SafeLong(4); val five = SafeLong(5)
      val start = 7
      val sigma = SafeLong(start + rng.nextInt(Int.MaxValue - start))
      val u = sigma * sigma - five
      val v = four * sigma

      val x = u * u * u
      val z = v * v * v

      val g = MulGroup.gen(n)   // TODO: memoize
      val t1 = g.inverse(SafeLong(4) * x * v)   // multiplicative inverse, can FAIL!

      val t2 = v - u
      val a = t2 * t2 * t2 * (SafeLong.three * u + v) * t1 - two
      MontgomeryCurve(a, zero, n, MontgomeryPoint(x % n, z % n))
    }

    def generate(n: Num): MontgomeryCurve = {
      genCurve(n)
    }
  }

  case class MontgomeryCurve(a: Num, b: Num, characteristic: Num, initialPoint: MontgomeryPoint) extends EllipticCurve[MontgomeryPoint] {
    import Utils._

    override val infinity: MontgomeryPoint = montgomeryInfinity

    override def neg(p: MontgomeryPoint): MontgomeryPoint = MontgomeryPoint(-p.x, p.z)

    override def double(p: MontgomeryPoint): MontgomeryPoint = {
      val xx = p.x * p.x
      val zz = p.z * p.z
      val xz = p.x * p.z
      val x = (xx - zz) ^ SafeLong.two % characteristic
      val y = 4 * xz * (xx + a * xz + zz) % characteristic
      MontgomeryPoint(x, y)
    }

    override def add(p1: MontgomeryPoint, p2: MontgomeryPoint): MontgomeryPoint = {
      ???
    }

    override def mul(p: MontgomeryPoint, multiple: Num): MontgomeryPoint = multiple match {
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
}

object MulGroup {
  def gen(n: Num): Group[Num] = new Group[Num]{
    override def inverse(a: Num): Num = SafeLong(a.toBigInt.modInverse(n.toBigInt))

    override def empty: Num = SafeLong.one

    override def combine(x: Num, y: Num): Num = x * y % n
  }

}
