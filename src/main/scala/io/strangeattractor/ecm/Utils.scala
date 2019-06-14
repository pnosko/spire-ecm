package io.strangeattractor.ecm

import scodec.bits._
import spire.math.SafeLong

object Utils {
  implicit class SafeLongOps(num: SafeLong) {
    def toBitVector: IndexedSeq[Boolean] = {
      BitVector(num.toBigInt.toByteArray).toIndexedSeq.dropWhile(!_)
    }
  }

  implicit class BigIntOps(num: BigInt) {
    def toSafeLong: SafeLong = SafeLong(num)
  }
}
