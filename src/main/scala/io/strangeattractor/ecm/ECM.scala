package io.strangeattractor.ecm

import io.strangeattractor.ecm.Alias.Num
import io.strangeattractor.ecm.Montgomery._
import io.strangeattractor.ecm.Utils._
import spire.math._
import spire.std.bigDecimal._
import spire.syntax.nroot._
import spire.syntax.trig._

object ECM {
  def factorECM(n: Num, curve: MontgomeryCurve, point: MontgomeryPoint): FactorizationResult = {
    val expectedFactorLength = n.sqrt().toBigDecimal.log(10).toInt
    factorizeMontgomery(n , getB1(expectedFactorLength), curve, point)
  }

  private def getB1(expectedFactorLength: Int): Long = expectedFactorLength match {
    case x if x < 70 => 2900000000L
    case x if x < 65 => 850000000L
    case x if x < 60 => 260000000L
    case x if x < 55 => 110000000L
    case x if x < 50 => 43000000L
    case x if x < 45 => 11000000L
    case x if x < 40 => 3000000L
    case x if x < 35 => 1000000L
    case x if x < 30 => 250000
    case x if x < 25 => 50000
    case x if x < 20 => 11000
    case x if x < 15 => 2000
    case x if x < 12 => 400
    case _ => 100L
  }

  private def factorizeMontgomery(n: Num, b1: Long, curve: MontgomeryCurve, point: MontgomeryPoint): FactorizationResult = {
    val arithmetic = Montgomery.arithmetic(curve, point)

    def findPower(base: Int): SafeLong = {
      val div = b1 / base
      var acc = base
      while(acc <= div) {
        acc = acc * base
      }
      SafeLong(acc)
    }

    val smallPrimes = primes.takeWhile(_ < b1)
    val smallPowers = smallPrimes.map(findPower)

    val multiple = smallPowers.foldLeft(point)(arithmetic.mul)

    val g = n.gcd(multiple.z)
    if (g.isOne) {
      // big step
      FactorizationResult.wrap(n)
    } else {
      FactorizationResult.fromSingleFactor(n, g)
    }
  }

  def factor(n: Num): FactorizationResult = {
    Montgomery.generate(n).fold(
      foundFactor => FactorizationResult.fromSingleFactor(n, foundFactor.n),
      {case (c, p) => factorECM(n, c, p)}
    )
  }
}
