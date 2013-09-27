package hep.aida.tfloat.ref

import hep.aida.tfloat.FloatIAxis
import hep.aida.tfloat.FloatIHistogram
import FloatVariableAxis._
//remove if not needed
import scala.collection.JavaConversions._

object FloatVariableAxis {

  /**
   * Returns a string representation of the specified array. The string
   * representation consists of a list of the arrays's elements, enclosed in
   * square brackets (<tt>"[]"</tt>). Adjacent elements are separated by the
   * characters <tt>", "</tt> (comma and space).
   *
   * @return a string representation of the specified array.
   */
  protected def toString(array: Array[Float]): String = {
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
class FloatVariableAxis(edges: Array[Float]) extends FloatIAxis {

  protected var min: Float = edges(0)

  protected var bins: Int = edges.length - 1

  protected var edges: Array[Float] = edges.clone()

  if (edges.length < 1) throw new IllegalArgumentException()

  for (i <- 0 until edges.length - 1 if edges(i + 1) <= edges(i)) {
    throw new IllegalArgumentException("edges must be sorted ascending and must not contain multiple identical values")
  }

  def binCentre(index: Int): Float = {
    (binLowerEdge(index) + binUpperEdge(index)) / 2
  }

  def binLowerEdge(index: Int): Float = {
    if (index == FloatIHistogram.UNDERFLOW) return Float.NEGATIVE_INFINITY
    if (index == FloatIHistogram.OVERFLOW) return upperEdge()
    edges(index)
  }

  def bins(): Int = bins

  def binUpperEdge(index: Int): Float = {
    if (index == FloatIHistogram.UNDERFLOW) return lowerEdge()
    if (index == FloatIHistogram.OVERFLOW) return Float.POSITIVE_INFINITY
    edges(index + 1)
  }

  def binWidth(index: Int): Float = {
    binUpperEdge(index) - binLowerEdge(index)
  }

  def coordToIndex(coord: Float): Int = {
    if (coord < min) return FloatIHistogram.UNDERFLOW
    var index = java.util.Arrays.binarySearch(this.edges, coord)
    if (index < 0) index = -index - 1 - 1
    if (index >= bins) return FloatIHistogram.OVERFLOW
    index
  }

  def lowerEdge(): Float = min

  def upperEdge(): Float = edges(edges.length - 1)
}
