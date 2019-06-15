package io.strangeattractor.ecm

import io.strangeattractor.ecm.ECM.FactorizationResult
import org.scalatest.{FlatSpec, Matchers}
import spire.math.SafeLong
import spire.random.Generator

class ECMTest extends FlatSpec with Matchers {
  import SafeLong._

  "Factor" should "return factors from curve generation" in {
    implicit val g: Generator = spire.random.rng.Serial.fromSeed(1L)
    val number = SafeLong(118L)
    val res = ECM.factor(number)

    res should be (FactorizationResult.fromSingleFactor(number, two))
  }

  "Factor" should "return factors from small powers" in {
    implicit val g: Generator = spire.random.rng.Serial.fromSeed(1L)
    val number = SafeLong(65)
    val res = ECM.factor(number)

    res should be (FactorizationResult.fromSingleFactor(number, SafeLong(5)))
  }

}
