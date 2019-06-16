package com.peterparameter.ecm

import Alias.Num
import Montgomery._
import Utils._
import spire.math._


object ECM {
  def factorECM(n: Num, curve: MontgomeryCurve, point: MontgomeryPoint): FactorizationResult = {
    factorizeMontgomery(n, b1Bound(n), curve, point)
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

  def factor(n: Num): FactorizationResult = Montgomery.generate(n).fold(
    foundFactor => FactorizationResult.fromSingleFactor(n, foundFactor.n),
    {case (c, p) => factorECM(n, c, p)}
  )
}
