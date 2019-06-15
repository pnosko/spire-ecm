package io.strangeattractor.ecm

import io.strangeattractor.ecm.Alias.Num
import scodec.bits._
import spire.math.SafeLong
import spire.std.bigDecimal._
import spire.syntax.nroot._
import spire.syntax.trig._
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

  private def expectedFactorLength(n: Num): Int = n.sqrt().toBigDecimal.log(10).toInt
  def b1Bound(n: Num): Long = getB1(expectedFactorLength(n))

  private def getB1(expectedFactorLength: Int): Long = expectedFactorLength match {
    case x if x < 70 => 2900000000L
    case x if x < 65 => 850000000L
    case x if x < 60 => 260000000L
    case x if x < 55 => 110000000L
    case x if x < 50 => 43000000L
    case x if x < 45 => 11000000L
    case x if x < 40 => 3000000L
    case x if x < 35 => 1000000L
    case x if x < 30 => 250000
    case x if x < 25 => 50000
    case x if x < 20 => 11000
    case x if x < 15 => 2000
    case x if x < 12 => 400
    case _ => 100L
  }
}
