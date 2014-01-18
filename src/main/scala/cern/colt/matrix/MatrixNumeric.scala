package cern.colt.matrix

import scala.math.Numeric._

object MatrixNumeric {
  trait MatrixNumeric[T] extends Numeric[T] {
    def div(a: T, b: T): T
    def fromDouble(d: Double): T
  }

  trait IntMatrixNumeric extends IntIsIntegral with MatrixNumeric[Int] {
    def div(x: Int, y: Int): Int = x / y
    def fromDouble(d: Double): Int = d.toInt
  }
  implicit object IntMatrixNumeric extends IntMatrixNumeric with scala.math.Ordering.IntOrdering

  trait ShortMatrixNumeric extends ShortIsIntegral with MatrixNumeric[Short] {
    def div(x: Short, y: Short): Short = (x / y).toShort
    def fromDouble(d: Double): Short = d.toShort
  }
  implicit object ShortMatrixNumeric extends ShortMatrixNumeric with scala.math.Ordering.ShortOrdering

  trait ByteMatrixNumeric extends ByteIsIntegral with MatrixNumeric[Byte] {
    def div(x: Byte, y: Byte): Byte = (x / y).toByte
    def fromDouble(d: Double): Byte = d.toByte
  }
  implicit object ByteMatrixNumeric extends ByteMatrixNumeric with scala.math.Ordering.ByteOrdering

  trait CharMatrixNumeric extends CharIsIntegral with MatrixNumeric[Char] {
    def div(x: Char, y: Char): Char = (x / y).toChar
    def fromDouble(d: Double): Char = d.toChar
  }
  implicit object CharMatrixNumeric extends CharMatrixNumeric with scala.math.Ordering.CharOrdering

  trait LongMatrixNumeric extends LongIsIntegral with MatrixNumeric[Long] {
    def div(x: Long, y: Long): Long = x / y
    def fromDouble(d: Double): Long = d.toLong
  }
  implicit object LongMatrixNumeric extends LongMatrixNumeric with scala.math.Ordering.LongOrdering

  trait FloatMatrixNumeric extends FloatIsFractional  with MatrixNumeric[Float] {
    def fromDouble(d: Double): Float = d.toFloat
  }
  implicit object FloatMatrixNumeric extends FloatMatrixNumeric with scala.math.Ordering.FloatOrdering

  trait DoubleMatrixNumeric extends DoubleIsFractional with MatrixNumeric[Double] {
    def fromDouble(d: Double): Double = d
  }
  implicit object DoubleMatrixNumeric extends DoubleMatrixNumeric with scala.math.Ordering.DoubleOrdering
}
