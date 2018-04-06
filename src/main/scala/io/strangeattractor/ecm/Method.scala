package io.strangeattractor.ecm

import io.strangeattractor.ecm.Curve._

/**
  * Created by Peter on 7/4/2017.
  */
object Method {

  trait FactorizationMethod

  trait FactorizationResolver[M <: FactorizationMethod] {
    type Point <: EllipticPoint
    type Curve <: EllipticCurve[Point]
    val generator: CurveGenerator[Point]
  }
}