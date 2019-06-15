package io.strangeattractor.ecm

import io.strangeattractor.ecm.Utils._
import org.scalatest.{FlatSpec, Matchers}
import spire.math.SafeLong
import spire.random.Generator

class MontgomeryTest extends FlatSpec with Matchers {
  import Montgomery._

  private def generator: Generator = spire.random.rng.Serial.fromSeed(1L)

  "toBitVector" should "convert to binary correctly" in {
    val number = 7L

    val binary = SafeLong(number).toBitVector
    val binaryString = number.toBinaryString
    binary.map(if(_) "1" else "0").mkString shouldEqual binaryString
  }

  "GenerateCurve" should "generate a curve when factorizing 13" in {
    val number = SafeLong(13L)
    val res = Montgomery.generate(number, generator)

    res.toOption.get._1.characteristic should be (number)
  }

  "GenerateCurve" should "return a factor when factorizing 118" in {
    val number = SafeLong(118L)
    val res = Montgomery.generate(number, generator)

    res.left.toOption.get.n should be (2L)
  }

  private def getArithmetic(number: SafeLong): MontgomeryArithmetic = {
    val res = Montgomery.generate(number, generator)

    val (curve, p) = res.toOption.get
    val arithmetic = new MontgomeryArithmetic(curve, p)
    arithmetic
  }

  "EllipticMultiply" should "return the same multiple as when adding/doubling for n = 3" in {
    val number = SafeLong(93L)
    val arithmetic = getArithmetic(number)
    val p = arithmetic.initialPoint

    val mul1 = arithmetic.mul(p, 3L)
    val d = p
    val dd = arithmetic.double(d)
    val mul2 = arithmetic.add(dd, p)(p)
    mul1 should be (mul2)
  }

  "EllipticMultiply" should "return the same multiple as when adding/doubling for n = 5" in {
    val number = SafeLong(93L)
    val arithmetic = getArithmetic(number)
    val p = arithmetic.initialPoint

    val mul1 = arithmetic.mul(p, 5L)
    val d = arithmetic.double(p)
    val dd = arithmetic.double(d)
    val mul2 = arithmetic.add(dd, p)(p)
    mul1 should be (mul2)
  }

  "EllipticMultiply" should "return the same multiple as when adding/doubling for n = 4" in {
    val number = SafeLong(93L)
    val arithmetic = getArithmetic(number)
    val p = arithmetic.initialPoint

    val mul1 = arithmetic.mul(p, 4L)
    val d = arithmetic.double(p)
    val dd = arithmetic.double(d)
    val mul2 = dd
    mul1 should be (mul2)
  }

  "EllipticMultiply" should "return the same multiple as when adding/doubling for n = 7" in {
    val number = SafeLong(1L)
    val arithmetic = getArithmetic(number)
    val p = arithmetic.initialPoint

    val mul1 = arithmetic.mul(p, 7L)
    val p2 = arithmetic.double(p)
    val p4 = arithmetic.double(p2)
    val p3 = arithmetic.add(p2, p)(p)
    val mul2 = arithmetic.add(p4, p3)(p)
    mul1 should be (mul2)
  }

  "EllipticMultiply" should "return the same multiple as when adding/doubling for n = 9" in {
    val number = SafeLong(93L)
    val arithmetic = getArithmetic(number)
    val p = arithmetic.initialPoint

    val mul1 = arithmetic.mul(p, 9L)
    val p2 = arithmetic.double(p)
    val p4 = arithmetic.double(p2)
    val p8 = arithmetic.double(p4)
    val mul2 = arithmetic.add(p8, p)(p)
    mul1 should be (mul2)
  }
}