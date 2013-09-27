package hep.aida.tfloat.ref

import hep.aida.tfloat.FloatIAxis
import hep.aida.tfloat.FloatIHistogram
import hep.aida.tfloat.FloatIHistogram2D
import hep.aida.tfloat.FloatIHistogram3D
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
abstract class FloatAbstractHistogram3D(title: String) extends FloatHistogram(title) with FloatIHistogram3D {

  protected var xAxis: FloatIAxis = _

  protected var yAxis: FloatIAxis = _

  protected var zAxis: FloatIAxis = _

  def allEntries(): Int = {
    var n = 0
    var i = xAxis.bins()
    while (i >= -2) var j = yAxis.bins()
    while (j >= -2) var k = zAxis.bins()
    while (k >= -2) {
      n += binEntries(i, j, k)
    }
    n
  }

  def dimensions(): Int = 3

  def entries(): Int = {
    var n = 0
    for (i <- 0 until xAxis.bins(); j <- 0 until yAxis.bins(); k <- 0 until zAxis.bins()) {
      n += binEntries(i, j, k)
    }
    n
  }

  def extraEntries(): Int = allEntries() - entries()

  def fill(x: Float, y: Float, z: Float) {
    fill(x, y, z, 1)
  }

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
  protected def internalSliceXY(title: String, indexZ1: Int, indexZ2: Int): FloatIHistogram2D

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
  protected def internalSliceXZ(title: String, indexY1: Int, indexY2: Int): FloatIHistogram2D

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
  protected def internalSliceYZ(title: String, indexX1: Int, indexX2: Int): FloatIHistogram2D

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

  /**
   * Package private method to map from the external representation of bin
   * number to our internal representation of bin number
   */
  def mapZ(index: Int): Int = {
    val bins = zAxis.bins() + 2
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
    var minBinZ = -1
    var maxBinX = -1
    var maxBinY = -1
    var maxBinZ = -1
    var i = xAxis.bins()
    while (i >= 0) {
      var j = yAxis.bins()
      while (j >= 0) {
        var k = zAxis.bins()
        while (k >= 0) {
          val value = binHeight(i, j, k)
          if (value < minValue) {
            minValue = value
            minBinX = i
            minBinY = j
            minBinZ = k
          }
          if (value > maxValue) {
            maxValue = value
            maxBinX = i
            maxBinY = j
            maxBinZ = k
          }
        }
      }
    }
    val result = Array(minBinX, minBinY, minBinZ, maxBinX, maxBinY, maxBinZ)
    result
  }

  def projectionXY(): FloatIHistogram2D = {
    val newTitle = title() + " (projectionXY)"
    internalSliceXY(newTitle, mapZ(FloatIHistogram.UNDERFLOW), mapZ(FloatIHistogram.OVERFLOW))
  }

  def projectionXZ(): FloatIHistogram2D = {
    val newTitle = title() + " (projectionXZ)"
    internalSliceXZ(newTitle, mapY(FloatIHistogram.UNDERFLOW), mapY(FloatIHistogram.OVERFLOW))
  }

  def projectionYZ(): FloatIHistogram2D = {
    val newTitle = title() + " (projectionYZ)"
    internalSliceYZ(newTitle, mapX(FloatIHistogram.UNDERFLOW), mapX(FloatIHistogram.OVERFLOW))
  }

  def sliceXY(indexZ: Int): FloatIHistogram2D = sliceXY(indexZ, indexZ)

  def sliceXY(indexZ1: Int, indexZ2: Int): FloatIHistogram2D = {
    val start = mapZ(indexZ1)
    val stop = mapZ(indexZ2)
    val newTitle = title() + " (sliceXY [" + indexZ1 + ":" + indexZ2 + "])"
    internalSliceXY(newTitle, start, stop)
  }

  def sliceXZ(indexY: Int): FloatIHistogram2D = sliceXZ(indexY, indexY)

  def sliceXZ(indexY1: Int, indexY2: Int): FloatIHistogram2D = {
    val start = mapY(indexY1)
    val stop = mapY(indexY2)
    val newTitle = title() + " (sliceXZ [" + indexY1 + ":" + indexY2 + "])"
    internalSliceXY(newTitle, start, stop)
  }

  def sliceYZ(indexX: Int): FloatIHistogram2D = sliceYZ(indexX, indexX)

  def sliceYZ(indexX1: Int, indexX2: Int): FloatIHistogram2D = {
    val start = mapX(indexX1)
    val stop = mapX(indexX2)
    val newTitle = title() + " (sliceYZ [" + indexX1 + ":" + indexX2 + "])"
    internalSliceYZ(newTitle, start, stop)
  }

  def sumAllBinHeights(): Float = {
    var n = 0
    var i = xAxis.bins()
    while (i >= -2) var j = yAxis.bins()
    while (j >= -2) var k = zAxis.bins()
    while (k >= -2) {
      n += binHeight(i, j, k)
    }
    n
  }

  def sumBinHeights(): Float = {
    var n = 0
    for (i <- 0 until xAxis.bins(); j <- 0 until yAxis.bins(); k <- 0 until zAxis.bins()) {
      n += binHeight(i, j, k)
    }
    n
  }

  def sumExtraBinHeights(): Float = sumAllBinHeights() - sumBinHeights()

  def xAxis(): FloatIAxis = xAxis

  def yAxis(): FloatIAxis = yAxis

  def zAxis(): FloatIAxis = zAxis
}
