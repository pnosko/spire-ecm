package com.peterparameter.ecm.typed

import com.peterparameter.ecm.common.Alias.Num

case class MontgomeryPoint[N <: Nat](x: Num, z: Num)

object MontgomeryPoint {

  implicit class MontgomeryPointOps[N <: Nat](p: MontgomeryPoint[N])(implicit arithmetic: MontgomeryArithmetic[N]) {
    def double: MontgomeryPoint[N] = arithmetic.double(p)

    def +(rhs: MontgomeryPoint[N])(implicit origin: MontgomeryPoint[N]): MontgomeryPoint[N] = arithmetic.add(p, rhs)(origin)

    def *(rhs: Num): MontgomeryPoint[N] = arithmetic.mul(p, rhs)
  }

}