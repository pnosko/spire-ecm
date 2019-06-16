package com.peterparameter.ecm

import Alias.Num
import scodec.bits._
import spire.math.SafeLong
import spire.std.bigDecimal._
import spire.syntax.nroot._
import spire.syntax.trig._

import scala.annotation.tailrec
import scala.collection.SeqView
import scala.collection.immutable.LazyList

object Utils {
  implicit class SafeLongOps(num: SafeLong) {
    def toBitVector: SeqView[Boolean] = {
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

  val primes: LazyList[Int] = {
    @tailrec
    def next(n: Int, stream: LazyList[Int]): LazyList[Int] =
      if (stream.isEmpty || (stream.head ^ 2) > n)
        n #:: loop(n + 2, primes)
      else if (n % stream.head == 0)
        next(n + 2, primes)
      else
        next(n, stream.tail)

    def loop(n: Int, stream: LazyList[Int]): LazyList[Int] = next(n, stream)

    2 #:: loop(3, primes)
  }

  private def expectedFactorLength(n: Num): Int = n.sqrt().toBigDecimal.log(10).toInt
  def b1Bound(n: Num): Long = getB1(expectedFactorLength(n))

  private def getB1(expectedFactorLength: Int): Long = expectedFactorLength match {
    case x if x > 65 => 2900000000L
    case x if x > 60 => 850000000L
    case x if x > 55 => 260000000L
    case x if x > 50 => 110000000L
    case x if x > 45 => 43000000L
    case x if x > 40 => 11000000L
    case x if x > 35 => 3000000L
    case x if x > 30 => 1000000L
    case x if x > 25 => 250000
    case x if x > 20 => 50000
    case x if x > 15 => 11000
    case x if x > 12 => 2000
    case _ => 400
  }
}
