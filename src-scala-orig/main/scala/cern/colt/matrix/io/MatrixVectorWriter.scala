package cern.colt.matrix.io

import java.io.OutputStream
import java.io.PrintWriter
import java.io.Writer
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Writes matrices and vectors
 */
class MatrixVectorWriter(out: OutputStream) extends PrintWriter(out) {

  /**
   * Constructor for MatrixVectorWriter
   *
   * @param out
   * @param autoFlush
   */
  def this(out: OutputStream, autoFlush: Boolean) {
    super(out, autoFlush)
  }

  /**
   * Constructor for MatrixVectorWriter
   *
   * @param out
   */
  def this(out: Writer) {
    super(out)
  }

  /**
   * Constructor for MatrixVectorWriter
   *
   * @param out
   * @param autoFlush
   */
  def this(out: Writer, autoFlush: Boolean) {
    super(out, autoFlush)
  }

  /**
   * Shifts the indexes. Useful for converting between 0- and 1-based
   * indicing.
   *
   * @param num
   *            Added to every index
   * @param indexes
   *            indexes to shift
   */
  def add(num: Int, indexes: Array[Int]) {
    for (i <- 0 until indexes.length) indexes(i) += num
  }

  /**
   * Prints the matrix info
   */
  def printMatrixInfo(info: MatrixInfo) {
    print(info.toString)
  }

  /**
   * Prints the vector info
   */
  def printVectorInfo(info: VectorInfo) {
    print(info.toString)
  }

  /**
   * Prints the matrix size
   */
  def printMatrixSize(size: MatrixSize, info: MatrixInfo) {
    format("%10d %10d", size.numRows(), size.numColumns())
    if (info.isCoordinate) format(" %19d", size.numEntries())
    println()
  }

  /**
   * Prints the matrix size. Assumes coordinate format
   */
  def printMatrixSize(size: MatrixSize) {
    format("%10d %10d %19d\n", size.numRows(), size.numColumns(), size.numEntries())
  }

  /**
   * Prints the vector size
   */
  def printVectorSize(size: VectorSize, info: VectorInfo) {
    format("%10d", size.size)
    if (info.isCoordinate) format(" %19d", size.numEntries())
    println()
  }

  /**
   * Prints the vector size. Assumes coordinate format
   */
  def printVectorSize(size: VectorSize) {
    format("%10d %19d\n", size.size, size.numEntries())
  }

  /**
   * Prints an array to the underlying stream. One entry per line.
   */
  def printArray(data: Array[Float]) {
    for (i <- 0 until data.length) format("% .12e\n", data(i))
  }

  /**
   * Prints an array to the underlying stream. One entry per line.
   */
  def printArray(data: Array[Double]) {
    for (i <- 0 until data.length) format("% .12e\n", data(i))
  }

  /**
   * Prints an array to the underlying stream. One entry per line. The first
   * array specifies the real entries, and the second is the imaginary entries
   */
  def printArray(dataR: Array[Float], dataI: Array[Float]) {
    val size = dataR.length
    if (size != dataI.length) throw new IllegalArgumentException("All arrays must be of the same size")
    for (i <- 0 until size) format("% .12e % .12e\n", dataR(i), dataI(i))
  }

  /**
   * Prints an array to the underlying stream. One entry per line. The first
   * array specifies the real entries, and the second is the imaginary entries
   */
  def printArray(dataR: Array[Double], dataI: Array[Double]) {
    val size = dataR.length
    if (size != dataI.length) throw new IllegalArgumentException("All arrays must be of the same size")
    for (i <- 0 until size) format("% .12e % .12e\n", dataR(i), dataI(i))
  }

  /**
   * Prints an array to the underlying stream. One entry per line.
   */
  def printArray(data: Array[Int]) {
    for (i <- 0 until data.length) format("%10d\n", data(i))
  }

  /**
   * Prints an array to the underlying stream. One entry per line.
   */
  def printArray(data: Array[Long]) {
    for (i <- 0 until data.length) format("%10d\n", data(i))
  }

  /**
   * Prints the coordinate format to the underlying stream. One index and
   * entry on each line. The offset is added to the index, typically, this can
   * transform from a 0-based indicing to a 1-based.
   */
  def printCoordinate(index: Array[Int], data: Array[Float], offset: Int) {
    val size = index.length
    if (size != data.length) throw new IllegalArgumentException("All arrays must be of the same size")
    for (i <- 0 until size) format("%10d % .12e\n", index(i) + offset, data(i))
  }

  /**
   * Prints the coordinate format to the underlying stream. One index and
   * entry on each line. The offset is added to the index, typically, this can
   * transform from a 0-based indicing to a 1-based.
   */
  def printCoordinate(index: Array[Int], data: Array[Double], offset: Int) {
    val size = index.length
    if (size != data.length) throw new IllegalArgumentException("All arrays must be of the same size")
    for (i <- 0 until size) format("%10d % .12e\n", index(i) + offset, data(i))
  }

  /**
   * Prints the coordinate format to the underlying stream. One index and
   * entry on each line. The offset is added to the index, typically, this can
   * transform from a 0-based indicing to a 1-based.
   */
  def printCoordinate(index: Array[Int], data: Array[Int], offset: Int) {
    val size = index.length
    if (size != data.length) throw new IllegalArgumentException("All arrays must be of the same size")
    for (i <- 0 until size) format("%10d %10d\n", index(i) + offset, data(i))
  }

  /**
   * Prints the coordinate format to the underlying stream. One index and
   * entry on each line. The offset is added to the index, typically, this can
   * transform from a 0-based indicing to a 1-based.
   */
  def printCoordinate(index: Array[Int], data: Array[Long], offset: Int) {
    val size = index.length
    if (size != data.length) throw new IllegalArgumentException("All arrays must be of the same size")
    for (i <- 0 until size) format("%10d %10d\n", index(i) + offset, data(i))
  }

  /**
   * Prints the coordinate format to the underlying stream. One index pair and
   * entry on each line. The offset is added to each index, typically, this
   * can transform from a 0-based indicing to a 1-based.
   */
  def printCoordinate(row: Array[Int], 
      column: Array[Int], 
      data: Array[Float], 
      offset: Int) {
    val size = row.length
    if (size != column.length || size != data.length) throw new IllegalArgumentException("All arrays must be of the same size")
    for (i <- 0 until size) format("%10d %10d % .12e\n", row(i) + offset, column(i) + offset, data(i))
  }

  /**
   * Prints the coordinate format to the underlying stream. One index pair and
   * entry on each line. The offset is added to each index, typically, this
   * can transform from a 0-based indicing to a 1-based.
   */
  def printCoordinate(row: Array[Int], 
      column: Array[Int], 
      data: Array[Double], 
      offset: Int) {
    val size = row.length
    if (size != column.length || size != data.length) throw new IllegalArgumentException("All arrays must be of the same size")
    for (i <- 0 until size) format("%10d %10d % .12e\n", row(i) + offset, column(i) + offset, data(i))
  }

  /**
   * Prints the coordinate format to the underlying stream. One index and
   * entry on each line. The offset is added to each index, typically, this
   * can transform from a 0-based indicing to a 1-based. The first float array
   * specifies the real entries, and the second is the imaginary entries
   */
  def printCoordinate(index: Array[Int], 
      dataR: Array[Float], 
      dataI: Array[Float], 
      offset: Int) {
    val size = index.length
    if (size != dataR.length || size != dataI.length) throw new IllegalArgumentException("All arrays must be of the same size")
    for (i <- 0 until size) format("%10d % .12e % .12e\n", index(i) + offset, dataR(i), dataI(i))
  }

  /**
   * Prints the coordinate format to the underlying stream. One index and
   * entry on each line. The offset is added to each index, typically, this
   * can transform from a 0-based indicing to a 1-based. The first double
   * array specifies the real entries, and the second is the imaginary entries
   */
  def printCoordinate(index: Array[Int], 
      dataR: Array[Double], 
      dataI: Array[Double], 
      offset: Int) {
    val size = index.length
    if (size != dataR.length || size != dataI.length) throw new IllegalArgumentException("All arrays must be of the same size")
    for (i <- 0 until size) format("%10d % .12e % .12e\n", index(i) + offset, dataR(i), dataI(i))
  }

  /**
   * Prints the coordinate format to the underlying stream. One index pair and
   * entry on each line. The offset is added to each index, typically, this
   * can transform from a 0-based indicing to a 1-based. The first float array
   * specifies the real entries, and the second is the imaginary entries
   */
  def printCoordinate(row: Array[Int], 
      column: Array[Int], 
      dataR: Array[Float], 
      dataI: Array[Float], 
      offset: Int) {
    val size = row.length
    if (size != column.length || size != dataR.length || size != dataI.length) throw new IllegalArgumentException("All arrays must be of the same size")
    for (i <- 0 until size) format("%10d %10d % .12e % .12e\n", row(i) + offset, column(i) + offset, 
      dataR(i), dataI(i))
  }

  /**
   * Prints the coordinate format to the underlying stream. One index pair and
   * entry on each line. The offset is added to each index, typically, this
   * can transform from a 0-based indicing to a 1-based. The first double
   * array specifies the real entries, and the second is the imaginary entries
   */
  def printCoordinate(row: Array[Int], 
      column: Array[Int], 
      dataR: Array[Double], 
      dataI: Array[Double], 
      offset: Int) {
    val size = row.length
    if (size != column.length || size != dataR.length || size != dataI.length) throw new IllegalArgumentException("All arrays must be of the same size")
    for (i <- 0 until size) format("%10d %10d % .12e % .12e\n", row(i) + offset, column(i) + offset, 
      dataR(i), dataI(i))
  }

  /**
   * Prints the coordinate format to the underlying stream. One index pair and
   * entry on each line. The offset is added to each index, typically, this
   * can transform from a 0-based indicing to a 1-based.
   */
  def printCoordinate(row: Array[Int], 
      column: Array[Int], 
      data: Array[Int], 
      offset: Int) {
    val size = row.length
    if (size != column.length || size != data.length) throw new IllegalArgumentException("All arrays must be of the same size")
    for (i <- 0 until size) format("%10d %10d %19d\n", row(i) + offset, column(i) + offset, data(i))
  }

  /**
   * Prints the coordinate format to the underlying stream. One index pair and
   * entry on each line. The offset is added to each index, typically, this
   * can transform from a 0-based indicing to a 1-based.
   */
  def printCoordinate(row: Array[Int], 
      column: Array[Int], 
      data: Array[Long], 
      offset: Int) {
    val size = row.length
    if (size != column.length || size != data.length) throw new IllegalArgumentException("All arrays must be of the same size")
    for (i <- 0 until size) format("%10d %10d %19d\n", row(i) + offset, column(i) + offset, data(i))
  }

  /**
   * Prints the coordinates to the underlying stream. One index pair on each
   * line. The offset is added to each index, typically, this can transform
   * from a 0-based indicing to a 1-based.
   */
  def printPattern(row: Array[Int], column: Array[Int], offset: Int) {
    val size = row.length
    if (size != column.length) throw new IllegalArgumentException("All arrays must be of the same size")
    for (i <- 0 until size) format("%10d %10d\n", row(i) + offset, column(i) + offset)
  }

  /**
   * Prints the coordinates to the underlying stream. One index on each line.
   * The offset is added to each index, typically, this can transform from a
   * 0-based indicing to a 1-based.
   */
  def printPattern(index: Array[Int], offset: Int) {
    val size = index.length
    for (i <- 0 until size) format("%10d\n", index(i) + offset)
  }

  /**
   * Prints the coordinate format to the underlying stream. One index pair and
   * entry on each line
   */
  def printCoordinate(row: Array[Int], column: Array[Int], data: Array[Float]) {
    printCoordinate(row, column, data, 0)
  }

  /**
   * Prints the coordinate format to the underlying stream. One index pair and
   * entry on each line
   */
  def printCoordinate(row: Array[Int], column: Array[Int], data: Array[Double]) {
    printCoordinate(row, column, data, 0)
  }

  /**
   * Prints the coordinate format to the underlying stream. One index pair and
   * entry on each line. The first double array specifies the real entries,
   * and the second is the imaginary entries
   */
  def printCoordinate(row: Array[Int], 
      column: Array[Int], 
      dataR: Array[Float], 
      dataI: Array[Float]) {
    printCoordinate(row, column, dataR, dataI, 0)
  }

  /**
   * Prints the coordinate format to the underlying stream. One index pair and
   * entry on each line. The first double array specifies the real entries,
   * and the second is the imaginary entries
   */
  def printCoordinate(row: Array[Int], 
      column: Array[Int], 
      dataR: Array[Double], 
      dataI: Array[Double]) {
    printCoordinate(row, column, dataR, dataI, 0)
  }

  /**
   * Prints the coordinate format to the underlying stream. One index pair and
   * entry on each line
   */
  def printCoordinate(row: Array[Int], column: Array[Int], data: Array[Int]) {
    printCoordinate(row, column, data, 0)
  }

  /**
   * Prints the coordinate format to the underlying stream. One index pair and
   * entry on each line
   */
  def printCoordinate(row: Array[Int], column: Array[Int], data: Array[Long]) {
    printCoordinate(row, column, data, 0)
  }

  /**
   * Prints the coordinates to the underlying stream. One index pair on each
   * line
   */
  def printPattern(row: Array[Int], column: Array[Int]) {
    printPattern(row, column, 0)
  }

  /**
   * Prints the coordinate format to the underlying stream. One index and
   * entry on each line
   */
  def printCoordinate(index: Array[Int], data: Array[Float]) {
    printCoordinate(index, data, 0)
  }

  /**
   * Prints the coordinate format to the underlying stream. One index and
   * entry on each line
   */
  def printCoordinate(index: Array[Int], data: Array[Double]) {
    printCoordinate(index, data, 0)
  }

  /**
   * Prints the coordinate format to the underlying stream. One index and
   * entry on each line. The first double array specifies the real entries,
   * and the second is the imaginary entries
   */
  def printCoordinate(index: Array[Int], dataR: Array[Float], dataI: Array[Float]) {
    printCoordinate(index, dataR, dataI, 0)
  }

  /**
   * Prints the coordinate format to the underlying stream. One index and
   * entry on each line. The first double array specifies the real entries,
   * and the second is the imaginary entries
   */
  def printCoordinate(index: Array[Int], dataR: Array[Double], dataI: Array[Double]) {
    printCoordinate(index, dataR, dataI, 0)
  }

  /**
   * Prints the coordinate format to the underlying stream. One index and
   * entry on each line
   */
  def printCoordinate(index: Array[Int], data: Array[Int]) {
    printCoordinate(index, data, 0)
  }

  /**
   * Prints the coordinate format to the underlying stream. One index and
   * entry on each line
   */
  def printCoordinate(index: Array[Int], data: Array[Long]) {
    printCoordinate(index, data, 0)
  }

  /**
   * Prints the coordinates to the underlying stream. One index on each line
   */
  def printPattern(index: Array[Int]) {
    printPattern(index, 0)
  }

  /**
   * Prints all the comments. Prepends a '%' and appends a newline to every
   * comment
   */
  def printComments(comments: Array[String]) {
    for (comment <- comments) println("%" + comment)
  }
}
