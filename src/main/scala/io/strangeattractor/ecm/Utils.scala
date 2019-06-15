package io.strangeattractor.ecm

import scodec.bits._
import spire.math.SafeLong

import scala.collection.SeqView

object Utils {
  implicit class SafeLongOps(num: SafeLong) {
    def toBitVector: SeqView[Boolean, IndexedSeq[Boolean]] = {
      val seq = BitVector.view(num.toBigInt.toByteArray).toIndexedSeq
      val idx = seq.indexOf(true)
      if (idx > 0)
        seq.view.slice(idx, seq.length)
      else seq.view
    }
  }

  implicit class BigIntOps(num: BigInt) {
    def toSafeLong: SafeLong = SafeLong(num)
  }
}
