package com.peterparameter.ecm.typed

import com.peterparameter.ecm.common.Alias.Num
import com.peterparameter.ecm.typed.Nat._
import spire.math.SafeLong

trait ToSafeLong[A <: Nat] {
  def value: Num
}

object ToSafeLong {
  def safeLongN[N <: Nat : ToSafeLong]: SafeLong = implicitly[ToSafeLong[N]].value

  implicit val zeroToLong: ToSafeLong[BNil.type] = new ToSafeLong[BNil.type] { val value: Num = 0L }

  implicit def zeroCase[T <: Nat](implicit n: ToSafeLong[T]): ToSafeLong[Zero[T]] = new ToSafeLong[Zero[T]] { val value: Num = 2 * n.value }

  implicit def oneCase[T <: Nat](implicit n: ToSafeLong[T]): ToSafeLong[One[T]] = new ToSafeLong[One[T]] { val value: Num = 2 * n.value + 1 }
}