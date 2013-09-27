package hep.aida.tfloat.ref

import hep.aida.tfloat.FloatIAxis
import hep.aida.tfloat.FloatIHistogram
import hep.aida.tfloat.FloatIHistogram1D
import hep.aida.tfloat.FloatIHistogram2D
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
abstract class FloatAbstractHistogram2D(title: String) extends FloatHistogram(title) with FloatIHistogram2D {

  protected var xAxis: FloatIAxis = _

  protected var yAxis: FloatIAxis = _

  def allEntries(): Int = {
    var n = 0
    var i = xAxis.bins()
    while (i >= -2) var j = yAxis.bins()
    while (j >= -2) {
      n += binEntries(i, j)
    }
    n
  }

  def binEntriesX(indexX: Int): Int = projectionX().binEntries(indexX)

  def binEntriesY(indexY: Int): Int = projectionY().binEntries(indexY)

  def binHeightX(indexX: Int): Float = projectionX().binHeight(indexX)

  def binHeightY(indexY: Int): Float = projectionY().binHeight(indexY)

  def dimensions(): Int = 2

  def entries(): Int = {
    var n = 0
    for (i <- 0 until xAxis.bins(); j <- 0 until yAxis.bins()) {
      n += binEntries(i, j)
    }
    n
  }

  def extraEntries(): Int = allEntries() - entries()

  def fill(x: Float, y: Float) {
    fill(x, y, 1)
  }

  /**
   * The precise meaning of the arguments to the public slice methods is
   * somewhat ambiguous, so we define this internal slice method and clearly
   * specify its arguments.
   * <p>
   * <b>Note 0</b>indexY1 and indexY2 use our INTERNAL bin numbering scheme
   * <b>Note 1</b>The slice is done between indexY1 and indexY2 INCLUSIVE
   * <b>Note 2</b>indexY1 and indexY2 may include the use of under and over
   * flow bins <b>Note 3</b>There is no note 3 (yet)
   */
  protected def internalSliceX(title: String, indexY1: Int, indexY2: Int): FloatIHistogram1D

  /**
   * The precise meaning of the arguments to the public slice methods is
   * somewhat ambiguous, so we define this internal slice method and clearly
   * specify its arguments.
   * <p>
   * <b>Note 0</b>indexX1 and indexX2 use our INTERNAL bin numbering scheme
   * <b>Note 1</b>The slice is done between indexX1 and indexX2 INCLUSIVE
   * <b>Note 2</b>indexX1 and indexX2 may include the use of under and over
   * flow bins <b>Note 3</b>There is no note 3 (yet)
   */
  protected def internalSliceY(title: String, indexX1: Int, indexX2: Int): FloatIHistogram1D

  /**
   * Package private method to map from the external representation of bin
   * number to our internal representation of bin number
   */
  def mapX(index: Int): Int = {
    val bins = xAxis.bins() + 2
    if (index >= bins) throw new IllegalArgumentException("bin=" + index)
    if (index >= 0) return index + 1
    if (index == FloatIHistogram.UNDERFLOW) return 0
    if (index == FloatIHistogram.OVERFLOW) return bins - 1
    throw new IllegalArgumentException("bin=" + index)
  }

  /**
   * Package private method to map from the external representation of bin
   * number to our internal representation of bin number
   */
  def mapY(index: Int): Int = {
    val bins = yAxis.bins() + 2
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
    var minBinY = -1
    var maxBinX = -1
    var maxBinY = -1
    var i = xAxis.bins()
    while (i >= 0) {
      var j = yAxis.bins()
      while (j >= 0) {
        val value = binHeight(i, j)
        if (value < minValue) {
          minValue = value
          minBinX = i
          minBinY = j
        }
        if (value > maxValue) {
          maxValue = value
          maxBinX = i
          maxBinY = j
        }
      }
    }
    val result = Array(minBinX, minBinY, maxBinX, maxBinY)
    result
  }

  def projectionX(): FloatIHistogram1D = {
    val newTitle = title() + " (projectionX)"
    internalSliceX(newTitle, mapY(FloatIHistogram.UNDERFLOW), mapY(FloatIHistogram.OVERFLOW))
  }

  def projectionY(): FloatIHistogram1D = {
    val newTitle = title() + " (projectionY)"
    internalSliceY(newTitle, mapX(FloatIHistogram.UNDERFLOW), mapX(FloatIHistogram.OVERFLOW))
  }

  def sliceX(indexY: Int): FloatIHistogram1D = {
    val start = mapY(indexY)
    val newTitle = title() + " (sliceX [" + indexY + "])"
    internalSliceX(newTitle, start, start)
  }

  def sliceX(indexY1: Int, indexY2: Int): FloatIHistogram1D = {
    val start = mapY(indexY1)
    val stop = mapY(indexY2)
    val newTitle = title() + " (sliceX [" + indexY1 + ":" + indexY2 + "])"
    internalSliceX(newTitle, start, stop)
  }

  def sliceY(indexX: Int): FloatIHistogram1D = {
    val start = mapX(indexX)
    val newTitle = title() + " (sliceY [" + indexX + "])"
    internalSliceY(newTitle, start, start)
  }

  def sliceY(indexX1: Int, indexX2: Int): FloatIHistogram1D = {
    val start = mapX(indexX1)
    val stop = mapX(indexX2)
    val newTitle = title() + " (slicey [" + indexX1 + ":" + indexX2 + "])"
    internalSliceY(newTitle, start, stop)
  }

  def sumAllBinHeights(): Float = {
    var n = 0
    var i = xAxis.bins()
    while (i >= -2) var j = yAxis.bins()
    while (j >= -2) {
      n += binHeight(i, j)
    }
    n
  }

  def sumBinHeights(): Float = {
    var n = 0
    for (i <- 0 until xAxis.bins(); j <- 0 until yAxis.bins()) {
      n += binHeight(i, j)
    }
    n
  }

  def sumExtraBinHeights(): Float = sumAllBinHeights() - sumBinHeights()

  def xAxis(): FloatIAxis = xAxis

  def yAxis(): FloatIAxis = yAxis
}
