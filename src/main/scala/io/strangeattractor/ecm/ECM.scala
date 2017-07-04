package io.strangeattractor.ecm

import io.strangeattractor.ecm.ECM._
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

  def factor[M <: FactorizationMethod : FactorizationResolver](n: Num): FactorizationResult = {

  }
}



