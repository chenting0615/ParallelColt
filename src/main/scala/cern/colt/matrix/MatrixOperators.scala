package cern.colt.matrix

import MatrixTypes._
import java.io.{ObjectInputStream, ObjectOutputStream}
import cern.colt.list.ArrayTypes.IntArrayList
import hep.aida.bin.{StatsResult, StaticBin1D}
import cern.colt.matrix.impl.{DenseMatrix1D, DenseMatrix2D}

/**
  */
object MatrixOperators {

  type StaticDoubleBin1D = StaticBin1D[Double]
  type StatsDoubleResult = StatsResult[Double]

  implicit def matrix2operatorsDouble(m : Matrix2D[Double]) = new Operators2D[Double](m)
  implicit def matrix2operatorsDouble(m : Matrix1D[Double]) = new Operators1D[Double](m)
  implicit def matrix2operatorsFloat(m : Matrix2D[Float]) = new Operators2D[Float](m)
  implicit def matrix2operatorsFloat(m : Matrix1D[Float]) = new Operators1D[Float](m)

  implicit def matrix2operatorsInt(m : IntArrayList) = new IntArrayOperators(m)

  def writeMatrix[T](out: ObjectOutputStream, matrix: Matrix2D[T]) {
    if (matrix == null)
      out.writeObject(null)
    else
      out.writeObject(matrix.toArray)
  }

  def writeMatrix[T](out: ObjectOutputStream, matrix: Matrix1D[T]) {
    if (matrix == null)
      out.writeObject(null)
    else
      out.writeObject(matrix.toArray)
  }

  def readMatrix2D[T: Manifest: Numeric](in: ObjectInputStream): Matrix2D[T] = {
    val values: Array[Array[T]] = in.readObject().asInstanceOf[Array[Array[T]]]
    if (values == null)
      null
    else
      new DenseMatrix2D[T](values)
  }

  def readMatrix1D[T: Manifest: Numeric](in: ObjectInputStream): Matrix1D[T] = {
    val values: Array[T] = in.readObject().asInstanceOf[Array[T]]
    if (values == null)
      null
    else
      new DenseMatrix1D[T](values)
  }

  def toMatrix2D[T: Manifest: Numeric]( values: Array[Array[T]] ) = {
    var maxWidth = 0
    values.foreach(row => if (row.size > maxWidth) maxWidth = row.size)
    val result = new DenseMatrix2D[T](values.size, maxWidth)
    var rowIdx = 0
    values.foreach(row => {
      var colIdx = 0
      row.foreach(value => {
        result.setQuick(rowIdx, colIdx, value)
        colIdx += 1
      })
      rowIdx += 1
    })
    result
  }

  def toMatrix1D[T: Manifest: Numeric]( values: Array[T] ) = {
    val result = new DenseMatrix1D[T](values.size)
    result.assign(values)
  }

  class IntArrayOperators(m: IntArrayList) {

    def take(indexes: IntMatrix1D) = {
      val result = new DenseIntMatrix1D(indexes.size.toInt)
      // No way to pass in the index using assign(), so just write it out.
      (0 until indexes.size.toInt).foreach(colIdx => {
        result.setQuick(colIdx, m.get(indexes.getQuick(colIdx)))
      })
      result
    }
  }
}
