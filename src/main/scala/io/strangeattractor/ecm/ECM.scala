package io.strangeattractor.ecm

import io.strangeattractor.ecm.Alias.Num
import io.strangeattractor.ecm.Montgomery._
import spire.math._

object ECM {

  case class FactorizationResult(factor: Num, rest: Num)

  def factorMontgomery(n: Num, curve: MontgomeryCurve, point: MontgomeryPoint): FactorizationResult = {
    val arithmetic = Montgomery.arithmetic(curve, point)


    FactorizationResult(n, SafeLong.one)
  }

  def factorECM(n: Num): FactorizationResult = {

    Montgomery.generate(n).fold(
      foundFactor => FactorizationResult(foundFactor.n, n / foundFactor.n),
      {case (c, p) => factorMontgomery(n, c, p)}
    )
  }
}
