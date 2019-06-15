package io.strangeattractor.ecm

import io.strangeattractor.ecm.Alias.Num
import io.strangeattractor.ecm.Montgomery._
import io.strangeattractor.ecm.Utils._
import spire.math._

object ECM {
  import SafeLong._

  case class FactorizationResult(factors: Factors, rest: Num)

  object FactorizationResult {
    def wrap(n: Num): FactorizationResult = FactorizationResult(Factors(n), one)
    def fromSingleFactor(n: Num, factor: Num): FactorizationResult = {
      val fs = factors(n, factor)
      val rest = n / fs.folded
      FactorizationResult(fs, rest)
    }
  }

  def factors(n: Num, g: Num): Factors = {
    val exp = timesDivisible(n, g)
    Factors(Map(g -> exp))
  }

  case class Factors(factors: Map[Num, Int]) {
    def folded: Num = factors.foldLeft(one){ case (acc, (b, e)) => acc * b ^ e}
  }

  object Factors {
    def apply(f: Num): Factors = Factors(Map(f -> 1))
  }

  def factorECM(n: Num, curve: MontgomeryCurve, point: MontgomeryPoint): FactorizationResult = {
    val b1 = 100000

    factorizeMontgomery(n , b1, curve, point)
  }

  def timesDivisible(n: Num, g: Num): Int = {
    import SafeLong._
    var exp = 0
    var acc = g
    while (n % acc === zero) {
      acc *= g
      exp += 1
    }
    exp
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
      FactorizationResult.wrap(n)
    } else {
      FactorizationResult.fromSingleFactor(n, g)
    }
  }

  def factor(n: Num): FactorizationResult = {
    Montgomery.generate(n).fold(
      foundFactor => {
        val fs = factors(n, foundFactor.n)
        FactorizationResult(fs, n / fs.folded)
      },
      {case (c, p) => factorECM(n, c, p)}
    )
  }
}
