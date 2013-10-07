package cern.colt.matrix

import cern.colt.function.FunctionTypes._
import cern.colt.matrix.Norm.Norm

/**
  */
trait MatrixAlgebra[T, M[T]] {

  def assign(value: M[T], func: DoubleDoubleFunction): M[T]

  def allNonZero: Boolean

  def +(value: T): M[T]

  def +=(value: T): M[T]

  def +(value: Matrix1D[T]): M[T]

  def -(value: T): M[T]

  def -=(value: T): M[T]

  def -(value: Matrix1D[T]): M[T]

  def -=(value: Matrix1D[T]): M[T]

  def *(factor: T): M[T]

  def *=(factor: T): M[T]

  def *(value: Matrix1D[T]): M[T]

  def *=(value: Matrix1D[T]): M[T]

  def /(divisor: T): M[T]

  def /=(divisor: T): M[T]

  def /(divisor: Matrix1D[T]): M[T]

  def /=(divisor: Matrix1D[T]): M[T]

  def **(value: Double): M[T]

  def **=(value: Double): M[T]

  def +=(value: Matrix1D[T]): M[T]

  def max: T

  def min: T

  def mean: Double

  def geoMean: Double

  def stdDev: Double

  def sqrt: M[T]

  def sqrtOnSelf: M[T]

  def exp: M[T]

  def expOnSelf: M[T]

  def log: M[T]

  def logOnSelf: M[T]

  def dot(other: Matrix1D[T]): M[T]

  def ==(value: T): M[Int]

  def !=(value: T): M[Int]

  def >(value: T): M[Int]

  def >=(value: T): M[Int]

  def <(value: T): M[Int]

  def <=(value: T): M[Int]

  def normalize(norm: Norm): M[T]

  def normalizeSelf(norm: Norm): M[T]

  def hasNaN: Boolean

  def locateNaNs(limit: Int = 10): String
}
