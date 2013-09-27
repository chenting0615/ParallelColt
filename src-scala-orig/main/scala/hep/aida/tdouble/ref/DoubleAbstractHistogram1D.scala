package hep.aida.tdouble.ref

import hep.aida.tdouble.DoubleIAxis
import hep.aida.tdouble.DoubleIHistogram
import hep.aida.tdouble.DoubleIHistogram1D
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
abstract class DoubleAbstractHistogram1D(title: String) extends DoubleHistogram(title) with DoubleIHistogram1D {

  protected var xAxis: DoubleIAxis = _

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
    if (index == DoubleIHistogram.UNDERFLOW) return 0
    if (index == DoubleIHistogram.OVERFLOW) return bins - 1
    throw new IllegalArgumentException("bin=" + index)
  }

  def minMaxBins(): Array[Int] = {
    var minValue = Double.MAX_VALUE
    var maxValue = Double.MIN_VALUE
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

  def sumAllBinHeights(): Double = sumBinHeights() + sumExtraBinHeights()

  def sumBinHeights(): Double = {
    var sum = 0
    var i = xAxis.bins()
    while (i >= 0) sum += binHeight(i)
    sum
  }

  def sumExtraBinHeights(): Double = {
    binHeight(UNDERFLOW) + binHeight(OVERFLOW)
  }

  def xAxis(): DoubleIAxis = xAxis
}
