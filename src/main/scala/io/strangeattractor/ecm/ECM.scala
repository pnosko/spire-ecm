package io.strangeattractor.ecm

import io.strangeattractor.ecm.Alias.Num
import io.strangeattractor.ecm.Curve._
import io.strangeattractor.ecm.Method._
import spire.math._

/**
  *
  */
object ECM {

  case class FactorizationResult(factor: Num, rest: Num)
  trait ECMAlgorithm {
    def factor[P <: EllipticPoint](n: Num, curve: EllipticCurve[P], initialPoint: P): FactorizationResult = {
      FactorizationResult(n, SafeLong.one)
    }
  }

  def factor[P <: EllipticPoint](n: Num, curve: EllipticCurve[P]): FactorizationResult = {
    FactorizationResult(n, SafeLong.one)
  }

  def factor[M <: FactorizationMethod : FactorizationResolver](n: Num): FactorizationResult = {
    val c = implicitly[FactorizationResolver[M]]

    val curve = c.generator.generate(n)

    factor(n, curve)
  }
}



