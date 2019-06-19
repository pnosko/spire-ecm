package com.peterparameter.ecm.typed

import cats.implicits._
import com.peterparameter.ecm.common.Alias.Num
import com.peterparameter.ecm.common.CurveUtils._
import com.peterparameter.ecm.common.Utils._
import com.peterparameter.ecm.common.{Factor, FactorizationResult}
import spire.math.SafeLong
import spire.random.Generator

object ECM {
  import SafeLong._
  import ToSafeLong._

  def factor[N <: Nat: ToSafeLong]: FactorizationResult = {
    ???
  }

  private def generateCurve[N <: Nat : ToSafeLong](rng: Generator = spire.random.GlobalRng): CurveResult[N] =
    generateCurveForSigma(getSigma(safeLongN[N], rng))

  case class MontgomeryCurve[N <: Nat](a: Num)
  type CurveResult[N <: Nat] = Either[Factor, (MontgomeryCurve[N], MontgomeryPoint[N])]
  private def generateCurveForSigma[N <: Nat : ToSafeLong](sigma: Num): CurveResult[N] = {
    val n = safeLongN[N]

    def modularInverse(number: Num): Option[Num] = Either.catchNonFatal(n.toBigInt.modInverse(n.toBigInt).toSafeLong).toOption

    val u = sigma * sigma - five
    val v = four * sigma

    val x = u * u * u % n
    val z = v * v * v % n

    val candidate = four * x * v % n

    def degenerate(candidate: Num): CurveResult[N] = {
      val gcd = candidate.gcd(n)
      if (gcd === n) generateCurve[N]()
      else Left(Factor(gcd))
    }

    def nonDegenerate(t1: Num): CurveResult[N] = {
      val t2 = (v - u + n) % n
      val a = (t2 * t2 * t2 * (three * u + v) * t1 - two) % n
      Right((MontgomeryCurve[N](a), MontgomeryPoint[N](x % n, z % n)))
    }

    val inverse = modularInverse(candidate)

    inverse.fold(degenerate(candidate))(nonDegenerate)
  }
}
