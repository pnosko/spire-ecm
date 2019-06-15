package io.strangeattractor.ecm

import io.strangeattractor.ecm.Alias.Num
import io.strangeattractor.ecm.Montgomery._
import io.strangeattractor.ecm.Utils._
import spire.math._

object ECM {

  case class FactorizationResult(factor: Num, rest: Num)

  def factorECM(n: Num, curve: MontgomeryCurve, point: MontgomeryPoint): FactorizationResult = {
    val b1 = 100000

    factorizeMontgomery(n , b1, curve, point)
  }

  private def factorizeMontgomery(n: Num, b1: Int, curve: MontgomeryCurve, point: MontgomeryPoint): FactorizationResult = {
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
      FactorizationResult(n, SafeLong.one)
    } else
      FactorizationResult(g, n / g)
  }

  def factor(n: Num): FactorizationResult = {

    Montgomery.generate(n).fold(
      foundFactor => FactorizationResult(foundFactor.n, n / foundFactor.n),
      {case (c, p) => factorECM(n, c, p)}
    )
  }
}
