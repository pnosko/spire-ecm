package io.strangeattractor.ecm

import org.scalatest.{FlatSpec, Matchers}
import spire.math.SafeLong
import spire.random.Generator

class MontgomeryTest extends FlatSpec with Matchers {
  import Montgomery._

  private def generator: Generator = spire.random.rng.Serial.fromSeed(1L)

  "GenerateCurve" should "generate a curve when factorizing 13" in {
    val gen = new MontgomeryGenerator(generator)
    val number = SafeLong(13L)
    val res = gen.generate(number)

    res.toOption.get._1.characteristic should be (number)
  }

  "GenerateCurve" should "return a factor when factorizing 118" in {
    val gen = new MontgomeryGenerator(generator)
    val number = SafeLong(118L)
    val res = gen.generate(number)

    res.left.toOption.get.n should be (2L)
  }

  "EllipticMultiply" should "return the same multiple as when adding/doubling for n = 3" in {
    val number = SafeLong(93L)
    val arithmetic = getArithmetic(number)
    val p = arithmetic.initialPoint

    val mul1 = arithmetic.mul(p, 3L)
    val d = p // curve.double(p)
    val dd = arithmetic.double(d)
    val mul2 = arithmetic.add(dd, p)
    mul1 should be (mul2)
  }

  private def getArithmetic(number: SafeLong): MontgomeryArithmetic = {
    val res = new MontgomeryGenerator(generator).generate(number)

    val (curve, p) = res.toOption.get
    val arithmetic = new MontgomeryArithmetic(curve, p)
    arithmetic
  }

  "EllipticMultiply" should "return the same multiple as when adding/doubling for n = 5" in {
    val gen = new MontgomeryGenerator(generator)
    val number = SafeLong(93L)
    val arithmetic = getArithmetic(number)
    val p = arithmetic.initialPoint

    val mul1 = arithmetic.mul(p, 5L)
    val d = arithmetic.double(p)
    val dd = arithmetic.double(d)
    val mul2 = arithmetic.add(dd, p)
    mul1 should be (mul2)
  }
}