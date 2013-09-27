package hep.aida.tfloat.ref

import hep.aida.tfloat.FloatIAxis
import hep.aida.tfloat.FloatIHistogram
import hep.aida.tfloat.FloatIHistogram1D
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Abstract base class extracting and implementing most of the redundancy of the
 * interface.
 *
 * @author Wolfgang Hoschek, Tony Johnson, and others.
 * @version 1.0, 23/03/2000
 */
@SerialVersionUID(1L)
abstract class FloatAbstractHistogram1D(title: String) extends FloatHistogram(title) with FloatIHistogram1D {

  protected var xAxis: FloatIAxis = _

  def allEntries(): Int = entries() + extraEntries()

  def dimensions(): Int = 1

  def entries(): Int = {
    var entries = 0
    var i = xAxis.bins()
    while (i >= 0) entries += binEntries(i)
    entries
  }

  def extraEntries(): Int = {
    binEntries(UNDERFLOW) + binEntries(OVERFLOW)
  }

  /**
   * Package private method to map from the external representation of bin
   * number to our internal representation of bin number
   */
  def map(index: Int): Int = {
    val bins = xAxis.bins() + 2
    if (index >= bins) throw new IllegalArgumentException("bin=" + index)
    if (index >= 0) return index + 1
    if (index == FloatIHistogram.UNDERFLOW) return 0
    if (index == FloatIHistogram.OVERFLOW) return bins - 1
    throw new IllegalArgumentException("bin=" + index)
  }

  def minMaxBins(): Array[Int] = {
    var minValue = Float.MAX_VALUE
    var maxValue = Float.MIN_VALUE
    var minBinX = -1
    var maxBinX = -1
    var i = xAxis.bins()
    while (i >= 0) {
      val value = binHeight(i)
      if (value < minValue) {
        minValue = value
        minBinX = i
      }
      if (value > maxValue) {
        maxValue = value
        maxBinX = i
      }
    }
    val result = Array(minBinX, maxBinX)
    result
  }

  def sumAllBinHeights(): Float = sumBinHeights() + sumExtraBinHeights()

  def sumBinHeights(): Float = {
    var sum = 0
    var i = xAxis.bins()
    while (i >= 0) sum += binHeight(i)
    sum
  }

  def sumExtraBinHeights(): Float = {
    binHeight(UNDERFLOW) + binHeight(OVERFLOW)
  }

  def xAxis(): FloatIAxis = xAxis
}
