package io.strangeattractor.ecm

import io.strangeattractor.ecm.Alias.Num
import io.strangeattractor.ecm.Curve._
import io.strangeattractor.ecm.Montgomery._
import spire.math._
import spire.random.Generator

/**
  *
  */
object ECM {

  case class FactorizationResult(factor: Num, rest: Num)

  def factorMontgomery(n: Num, curve: MontgomeryCurve, point: MontgomeryPoint): FactorizationResult = {
    FactorizationResult(n, SafeLong.one)
  }

  def factor(n: Num)(implicit g: Generator): FactorizationResult = {
    val gen = new MontgomeryGenerator(g)

    gen.generate(n).fold(
      foundFactor => FactorizationResult(foundFactor.n, n / foundFactor.n),
      {case (c, p) => factorMontgomery(n, c, p)}
    )
  }
}
