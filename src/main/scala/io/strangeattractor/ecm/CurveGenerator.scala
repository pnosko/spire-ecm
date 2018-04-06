package io.strangeattractor.ecm

import io.strangeattractor.ecm.Alias.Num
import io.strangeattractor.ecm.Curve._

/**
  * Created by Peter on 7/4/2017.
  */
trait CurveGenerator[P <: EllipticPoint] {
  def generate(n: Num): EllipticCurve[P]
}
