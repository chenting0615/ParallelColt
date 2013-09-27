package hep.aida.tdouble.ref

import hep.aida.tdouble.DoubleIAxis
import hep.aida.tdouble.DoubleIHistogram
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Fixed-width axis; A reference implementation of hep.aida.IAxis.
 *
 * @author Wolfgang Hoschek, Tony Johnson, and others.
 * @version 1.0, 23/03/2000
 */
@SerialVersionUID(1L)
class DoubleFixedAxis(var bins: Int, var min: Double, max: Double) extends DoubleIAxis {

  private var binWidth: Double = (max - min) / bins

  private var xunder: Int = _

  private var xover: Int = _

  if (bins < 1) throw new IllegalArgumentException("bins=" + bins)

  if (max <= min) throw new IllegalArgumentException("max <= min")

  def binCentre(index: Int): Double = min + binWidth * index + binWidth / 2

  def binLowerEdge(index: Int): Double = {
    if (index == DoubleIHistogram.UNDERFLOW) return Double.NEGATIVE_INFINITY
    if (index == DoubleIHistogram.OVERFLOW) return upperEdge()
    min + binWidth * index
  }

  def binUpperEdge(index: Int): Double = {
    if (index == DoubleIHistogram.UNDERFLOW) return min
    if (index == DoubleIHistogram.OVERFLOW) return Double.POSITIVE_INFINITY
    min + binWidth * (index + 1)
  }

  def binWidth(index: Int): Double = binWidth

  def coordToIndex(coord: Double): Int = {
    if (coord < min) return DoubleIHistogram.UNDERFLOW
    val index = Math.floor((coord - min) / binWidth).toInt
    if (index >= bins) return DoubleIHistogram.OVERFLOW
    index
  }

  def lowerEdge(): Double = min

  def upperEdge(): Double = min + binWidth * bins

  /**
   * This package private method is similar to coordToIndex except that it
   * returns our internal definition for overflow/underflow
   */
  def xgetBin(coord: Double): Int = {
    if (coord < min) return xunder
    val index = Math.floor((coord - min) / binWidth).toInt
    if (index > bins) return xover
    index + 1
  }

  /**
   * Package private method to map from the external representation of bin
   * number to our internal representation of bin number
   */
  def xmap(index: Int): Int = {
    if (index >= bins) throw new IllegalArgumentException("bin=" + index)
    if (index >= 0) return index + 1
    if (index == DoubleIHistogram.UNDERFLOW) return xunder
    if (index == DoubleIHistogram.OVERFLOW) return xover
    throw new IllegalArgumentException("bin=" + index)
  }
}
