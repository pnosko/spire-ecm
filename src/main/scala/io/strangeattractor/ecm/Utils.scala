package io.strangeattractor.ecm

import scodec.bits._
import spire.math.SafeLong

import scala.annotation.tailrec
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

  val primes: Stream[Int] = {
    @tailrec
    def next(n: Int, stream: Stream[Int]): Stream[Int] =
      if (stream.isEmpty || (stream.head ^ 2) > n)
        n #:: loop(n + 2, primes)
      else if (n % stream.head == 0)
        next(n + 2, primes)
      else
        next(n, stream.tail)

    def loop(n: Int, stream: Stream[Int]): Stream[Int] = next(n, stream)

    2 #:: loop(3, primes)
  }
}
