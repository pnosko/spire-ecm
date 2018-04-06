package io.strangeattractor.ecm

import org.scalatest.{FlatSpec, Matchers}
import spire.math.SafeLong

class MontgomeryTest extends FlatSpec with Matchers {
  import Montgomery._

  private val number = SafeLong.ten + SafeLong.one

  it should "generate a curve" in {
    val gen = new MontgomeryGenerator
    val c = gen.generate(number)

    c.characteristic should be (number)
  }
}