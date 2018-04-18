package io.strangeattractor.ecm

import scodec.bits._
import spire.math.SafeLong

object Utils {
  implicit class SafeLongOps(num: SafeLong) {
    def toBitVector: BitVector = {
      BitVector(num.toBigInt.toByteArray)
    }
  }
}
