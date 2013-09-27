package hep.aida.tdouble.ref

import hep.aida.tdouble.DoubleIAxis
import hep.aida.tdouble.DoubleIHistogram
import hep.aida.tdouble.DoubleIHistogram2D
import hep.aida.tdouble.DoubleIHistogram3D
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
abstract class DoubleAbstractHistogram3D(title: String) extends DoubleHistogram(title) with DoubleIHistogram3D {

  protected var xAxis: DoubleIAxis = _

  protected var yAxis: DoubleIAxis = _

  protected var zAxis: DoubleIAxis = _

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

  def fill(x: Double, y: Double, z: Double) {
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
  protected def internalSliceXY(title: String, indexZ1: Int, indexZ2: Int): DoubleIHistogram2D

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
  protected def internalSliceXZ(title: String, indexY1: Int, indexY2: Int): DoubleIHistogram2D

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
  protected def internalSliceYZ(title: String, indexX1: Int, indexX2: Int): DoubleIHistogram2D

  /**
   * Package private method to map from the external representation of bin
   * number to our internal representation of bin number
   */
  def mapX(index: Int): Int = {
    val bins = xAxis.bins() + 2
    if (index >= bins) throw new IllegalArgumentException("bin=" + index)
    if (index >= 0) return index + 1
    if (index == DoubleIHistogram.UNDERFLOW) return 0
    if (index == DoubleIHistogram.OVERFLOW) return bins - 1
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
    if (index == DoubleIHistogram.UNDERFLOW) return 0
    if (index == DoubleIHistogram.OVERFLOW) return bins - 1
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
    if (index == DoubleIHistogram.UNDERFLOW) return 0
    if (index == DoubleIHistogram.OVERFLOW) return bins - 1
    throw new IllegalArgumentException("bin=" + index)
  }

  def minMaxBins(): Array[Int] = {
    var minValue = Double.MAX_VALUE
    var maxValue = Double.MIN_VALUE
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

  def projectionXY(): DoubleIHistogram2D = {
    val newTitle = title() + " (projectionXY)"
    internalSliceXY(newTitle, mapZ(DoubleIHistogram.UNDERFLOW), mapZ(DoubleIHistogram.OVERFLOW))
  }

  def projectionXZ(): DoubleIHistogram2D = {
    val newTitle = title() + " (projectionXZ)"
    internalSliceXZ(newTitle, mapY(DoubleIHistogram.UNDERFLOW), mapY(DoubleIHistogram.OVERFLOW))
  }

  def projectionYZ(): DoubleIHistogram2D = {
    val newTitle = title() + " (projectionYZ)"
    internalSliceYZ(newTitle, mapX(DoubleIHistogram.UNDERFLOW), mapX(DoubleIHistogram.OVERFLOW))
  }

  def sliceXY(indexZ: Int): DoubleIHistogram2D = sliceXY(indexZ, indexZ)

  def sliceXY(indexZ1: Int, indexZ2: Int): DoubleIHistogram2D = {
    val start = mapZ(indexZ1)
    val stop = mapZ(indexZ2)
    val newTitle = title() + " (sliceXY [" + indexZ1 + ":" + indexZ2 + "])"
    internalSliceXY(newTitle, start, stop)
  }

  def sliceXZ(indexY: Int): DoubleIHistogram2D = sliceXZ(indexY, indexY)

  def sliceXZ(indexY1: Int, indexY2: Int): DoubleIHistogram2D = {
    val start = mapY(indexY1)
    val stop = mapY(indexY2)
    val newTitle = title() + " (sliceXZ [" + indexY1 + ":" + indexY2 + "])"
    internalSliceXY(newTitle, start, stop)
  }

  def sliceYZ(indexX: Int): DoubleIHistogram2D = sliceYZ(indexX, indexX)

  def sliceYZ(indexX1: Int, indexX2: Int): DoubleIHistogram2D = {
    val start = mapX(indexX1)
    val stop = mapX(indexX2)
    val newTitle = title() + " (sliceYZ [" + indexX1 + ":" + indexX2 + "])"
    internalSliceYZ(newTitle, start, stop)
  }

  def sumAllBinHeights(): Double = {
    var n = 0
    var i = xAxis.bins()
    while (i >= -2) var j = yAxis.bins()
    while (j >= -2) var k = zAxis.bins()
    while (k >= -2) {
      n += binHeight(i, j, k)
    }
    n
  }

  def sumBinHeights(): Double = {
    var n = 0
    for (i <- 0 until xAxis.bins(); j <- 0 until yAxis.bins(); k <- 0 until zAxis.bins()) {
      n += binHeight(i, j, k)
    }
    n
  }

  def sumExtraBinHeights(): Double = sumAllBinHeights() - sumBinHeights()

  def xAxis(): DoubleIAxis = xAxis

  def yAxis(): DoubleIAxis = yAxis

  def zAxis(): DoubleIAxis = zAxis
}
