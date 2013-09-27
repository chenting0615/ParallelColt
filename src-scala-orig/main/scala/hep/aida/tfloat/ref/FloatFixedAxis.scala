package hep.aida.tfloat.ref

import hep.aida.tfloat.FloatIAxis
import hep.aida.tfloat.FloatIHistogram
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Fixed-width axis; A reference implementation of hep.aida.IAxis.
 *
 * @author Wolfgang Hoschek, Tony Johnson, and others.
 * @version 1.0, 23/03/2000
 */
@SerialVersionUID(1L)
class FloatFixedAxis(var bins: Int, var min: Float, max: Float) extends FloatIAxis {

  private var binWidth: Float = (max - min) / bins

  private var xunder: Int = _

  private var xover: Int = _

  if (bins < 1) throw new IllegalArgumentException("bins=" + bins)

  if (max <= min) throw new IllegalArgumentException("max <= min")

  def binCentre(index: Int): Float = min + binWidth * index + binWidth / 2

  def binLowerEdge(index: Int): Float = {
    if (index == FloatIHistogram.UNDERFLOW) return Float.NEGATIVE_INFINITY
    if (index == FloatIHistogram.OVERFLOW) return upperEdge()
    min + binWidth * index
  }

  def binUpperEdge(index: Int): Float = {
    if (index == FloatIHistogram.UNDERFLOW) return min
    if (index == FloatIHistogram.OVERFLOW) return Float.POSITIVE_INFINITY
    min + binWidth * (index + 1)
  }

  def binWidth(index: Int): Float = binWidth

  def coordToIndex(coord: Float): Int = {
    if (coord < min) return FloatIHistogram.UNDERFLOW
    val index = Math.floor((coord - min) / binWidth).toInt
    if (index >= bins) return FloatIHistogram.OVERFLOW
    index
  }

  def lowerEdge(): Float = min

  def upperEdge(): Float = min + binWidth * bins

  /**
   * This package private method is similar to coordToIndex except that it
   * returns our internal definition for overflow/underflow
   */
  def xgetBin(coord: Float): Int = {
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
    if (index == FloatIHistogram.UNDERFLOW) return xunder
    if (index == FloatIHistogram.OVERFLOW) return xover
    throw new IllegalArgumentException("bin=" + index)
  }
}
