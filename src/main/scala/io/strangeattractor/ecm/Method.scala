package io.strangeattractor.ecm

import io.strangeattractor.ecm.ECM._

/**
  * Created by Peter on 7/4/2017.
  */
object Method {

  sealed trait FactorizationMethod
  trait MontgomeryMethod extends FactorizationMethod

  trait FactorizationResolver[M <: FactorizationMethod] {
    type Point <: EllipticPoint
    type Curve <: EllipticCurve[Point]
    type Gen <: CurveGenerator[Point]
  }

  implicit val montgomery = new FactorizationResolver[MontgomeryMethod] {
    type Point = MontgomeryPoint
    type Curve = MontgomeryCurve
    type Gen = MontgomeryGenerator
  }
}