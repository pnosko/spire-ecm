package com.peterparameter.ecm.common

import com.peterparameter.ecm.common.Alias.Num
import spire.math.SafeLong.one

case class Factors(factors: Map[Num, Int]) {
  def folded: Num = factors.foldLeft(one){ case (acc, (b, e)) => acc * b ^ e}
}

object Factors {
  def apply(f: Num): Factors = Factors(Map(f -> 1))
}