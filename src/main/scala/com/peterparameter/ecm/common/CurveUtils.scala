package com.peterparameter.ecm.common

import Alias.Num
import Utils._
import spire.math._
import spire.random.{Dist, Generator}

object CurveUtils {
  val four = SafeLong(4)
  val five = SafeLong(5)

  def getSigma(n: Num, rng: Generator): Num = {
    val byteLength = max(1, n.bitLength / 8)
    val dist = Dist.bigint(byteLength)

    val start = SafeLong(7)
    val random = rng.next[BigInt](dist).toSafeLong % (n - start)
    val sigma = start + random
    sigma
  }
}
