package io.strangeattractor.ecm

import spire.math.{SafeLong, SafeLongBigInteger, SafeLongLong}
import scodec.bits._

object Utils {
  implicit class SafeLongOps(num: SafeLong) {
    def toBitVector: BitVector = {
      BitVector(1L)
      //      }num match {
      //      case SafeLongLong(n) => BitVector(n)
      //      case SafeLongBigInteger(n) => BitVector(n.toByteArray)
      //    }
    }
  }
}
