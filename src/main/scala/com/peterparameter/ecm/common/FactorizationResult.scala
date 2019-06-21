package com.peterparameter.ecm.common

import com.peterparameter.ecm.common
import com.peterparameter.ecm.common.Alias.Num
import spire.math.SafeLong
import spire.math.SafeLong.one

case class FactorizationResult(factors: Factors, rest: Num)

object FactorizationResult {
  def wrap(n: Num): FactorizationResult = FactorizationResult(Factors(n), one)
  def fromSingleFactor(n: Num, factor: Num): FactorizationResult = {
    val fs = factors(n, factor)
    val rest = n / fs.folded
    FactorizationResult(fs, rest)
  }

  private def factors(n: Num, g: Num): Factors = {
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

    val exp = timesDivisible(n, g)
    common.Factors(Map(g -> exp))
  }
}