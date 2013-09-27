package hep.aida.tdouble.ref

import hep.aida.tdouble.DoubleIAxis
import hep.aida.tdouble.DoubleIHistogram
import DoubleVariableAxis._
//remove if not needed
import scala.collection.JavaConversions._

object DoubleVariableAxis {

  /**
   * Returns a string representation of the specified array. The string
   * representation consists of a list of the arrays's elements, enclosed in
   * square brackets (<tt>"[]"</tt>). Adjacent elements are separated by the
   * characters <tt>", "</tt> (comma and space).
   *
   * @return a string representation of the specified array.
   */
  protected def toString(array: Array[Double]): String = {
    val buf = new StringBuffer()
    buf.append("[")
    val maxIndex = array.length - 1
    var i = 0
    while (i <= maxIndex) {
      buf.append(array(i))
      if (i < maxIndex) buf.append(", ")
      i += 1
    }
    buf.append("]")
    buf.toString
  }
}

/**
 * Variable-width axis; A reference implementation of hep.aida.IAxis.
 *
 * @author Wolfgang Hoschek, Tony Johnson, and others.
 * @version 1.0, 23/03/2000
 */
@SerialVersionUID(1L)
class DoubleVariableAxis(edges: Array[Double]) extends DoubleIAxis {

  protected var min: Double = edges(0)

  protected var bins: Int = edges.length - 1

  protected var edges: Array[Double] = edges.clone()

  if (edges.length < 1) throw new IllegalArgumentException()

  for (i <- 0 until edges.length - 1 if edges(i + 1) <= edges(i)) {
    throw new IllegalArgumentException("edges must be sorted ascending and must not contain multiple identical values")
  }

  def binCentre(index: Int): Double = {
    (binLowerEdge(index) + binUpperEdge(index)) / 2
  }

  def binLowerEdge(index: Int): Double = {
    if (index == DoubleIHistogram.UNDERFLOW) return Double.NEGATIVE_INFINITY
    if (index == DoubleIHistogram.OVERFLOW) return upperEdge()
    edges(index)
  }

  def bins(): Int = bins

  def binUpperEdge(index: Int): Double = {
    if (index == DoubleIHistogram.UNDERFLOW) return lowerEdge()
    if (index == DoubleIHistogram.OVERFLOW) return Double.POSITIVE_INFINITY
    edges(index + 1)
  }

  def binWidth(index: Int): Double = {
    binUpperEdge(index) - binLowerEdge(index)
  }

  def coordToIndex(coord: Double): Int = {
    if (coord < min) return DoubleIHistogram.UNDERFLOW
    var index = java.util.Arrays.binarySearch(this.edges, coord)
    if (index < 0) index = -index - 1 - 1
    if (index >= bins) return DoubleIHistogram.OVERFLOW
    index
  }

  def lowerEdge(): Double = min

  def upperEdge(): Double = edges(edges.length - 1)
}
