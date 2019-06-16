package com.peterparameter.ecm

import com.peterparameter.ecm.common.Utils._
import com.peterparameter.ecm.basic.Montgomery
import org.scalatest.{FlatSpec, Matchers}
import spire.math.SafeLong
import spire.random.Generator

class MontgomeryTest extends FlatSpec with Matchers {
  import com.peterparameter.ecm.basic.Montgomery._

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

    val mul3 = arithmetic.mul(p, 3L)
    val p2 = arithmetic.double(p)
    val p3 = arithmetic.add(p2, p)(p)
    mul3 should be (p3)
  }

  "EllipticMultiply" should "return the same multiple as when adding/doubling for n = 5" in {
    val number = SafeLong(93L)
    val arithmetic = getArithmetic(number)
    val p = arithmetic.initialPoint

    val mul5 = arithmetic.mul(p, 5L)
    val p2 = arithmetic.double(p)
    val p3 = arithmetic.add(p2, p)(p)
    val p5 = arithmetic.add(p3, p2)(p)
    mul5 should be (p5)
  }

  "EllipticMultiply" should "return the same multiple as when adding/doubling for n = 4" in {
    val number = SafeLong(93L)
    val arithmetic = getArithmetic(number)
    val p = arithmetic.initialPoint

    val mul4 = arithmetic.mul(p, 4L)
    val p2 = arithmetic.double(p)
    val p4 = arithmetic.double(p2)
    mul4 should be (p4)
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

  "EllipticMultiply" should "return the same multiple as when adding/doubling for n = 13" in {
    val number = SafeLong(93L)
    val arithmetic = getArithmetic(number)
    val p = arithmetic.initialPoint

    val mul13 = arithmetic.mul(p, 13L)
    val p2 = arithmetic.double(p)
    val p3 = arithmetic.add(p2, p)(p)
    val p4 = arithmetic.double(p2)
    val p6 = arithmetic.double(p3)
    val p7 = arithmetic.add(p4, p3)(p)
    val mul13x = arithmetic.add(p7, p6)(p)
    mul13 should be (mul13x)
  }
}