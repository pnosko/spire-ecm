package com.peterparameter.ecm.typed

sealed trait Nat

object Nat {

  case object BNil extends Nat

  trait One[A <: Nat] extends Nat
  trait Zero[A <: Nat] extends Nat

  type one = One[BNil.type]
  type two = Zero[one]
  type three = One[one]
}
