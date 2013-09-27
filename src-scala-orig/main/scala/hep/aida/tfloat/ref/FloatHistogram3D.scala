package hep.aida.tfloat.ref

import hep.aida.tfloat.FloatIAxis
import hep.aida.tfloat.FloatIHistogram2D
import hep.aida.tfloat.FloatIHistogram3D
//remove if not needed
import scala.collection.JavaConversions._

/**
 * A reference implementation of hep.aida.IHistogram3D. The goal is to provide a
 * clear implementation rather than the most efficient implementation. However,
 * performance seems fine - filling 3 * 10^5 points/sec, both using FixedAxis or
 * VariableAxis.
 *
 * @author Wolfgang Hoschek, Tony Johnson, and others.
 * @version 1.0, 23/03/2000
 */
@SerialVersionUID(1L)
class FloatHistogram3D(title: String, 
    xAxis: FloatIAxis, 
    yAxis: FloatIAxis, 
    zAxis: FloatIAxis) extends FloatAbstractHistogram3D(title) with FloatIHistogram3D {

  private var heights: Array[Array[Array[Float]]] = new Array[Array[Array[Float]]](xBins + 2, yBins + 2, zBins + 2)

  private var errors: Array[Array[Array[Float]]] = new Array[Array[Array[Float]]](xBins + 2, yBins + 2, zBins + 2)

  private var entries: Array[Array[Array[Int]]] = new Array[Array[Array[Int]]](xBins + 2, yBins + 2, zBins + 2)

  private var nEntry: Int = _

  private var sumWeight: Float = _

  private var sumWeightSquared: Float = _

  private var meanX: Float = _

  private var rmsX: Float = _

  private var meanY: Float = _

  private var rmsY: Float = _

  private var meanZ: Float = _

  private var rmsZ: Float = _

  this.xAxis = xAxis

  this.yAxis = yAxis

  this.zAxis = zAxis

  val xBins = xAxis.bins()

  val yBins = yAxis.bins()

  val zBins = zAxis.bins()

  /**
   * Creates a variable-width histogram. Example:
   * <tt>xEdges = (0.2, 1.0, 5.0, 6.0), yEdges = (-5, 0, 7), zEdges = (-5, 0, 7)</tt>
   * yields 3*2*2 in-range bins.
   *
   * @param title
   *            The histogram title.
   * @param xEdges
   *            the bin boundaries the x-axis shall have; must be sorted
   *            ascending and must not contain multiple identical elements.
   * @param yEdges
   *            the bin boundaries the y-axis shall have; must be sorted
   *            ascending and must not contain multiple identical elements.
   * @param zEdges
   *            the bin boundaries the z-axis shall have; must be sorted
   *            ascending and must not contain multiple identical elements.
   * @throws IllegalArgumentException
   *             if
   *             <tt>xEdges.length < 1 || yEdges.length < 1|| zEdges.length < 1</tt>
   *             .
   */
  def this(title: String, 
      xEdges: Array[Float], 
      yEdges: Array[Float], 
      zEdges: Array[Float]) {
    this(title, new FloatVariableAxis(xEdges), new FloatVariableAxis(yEdges), new FloatVariableAxis(zEdges))
  }

  /**
   * Creates a fixed-width histogram.
   *
   * @param title
   *            The histogram title.
   * @param xBins
   *            The number of bins on the X axis.
   * @param xMin
   *            The minimum value on the X axis.
   * @param xMax
   *            The maximum value on the X axis.
   * @param yBins
   *            The number of bins on the Y axis.
   * @param yMin
   *            The minimum value on the Y axis.
   * @param yMax
   *            The maximum value on the Y axis.
   * @param zBins
   *            The number of bins on the Z axis.
   * @param zMin
   *            The minimum value on the Z axis.
   * @param zMax
   *            The maximum value on the Z axis.
   */
  def this(title: String, 
      xBins: Int, 
      xMin: Float, 
      xMax: Float, 
      yBins: Int, 
      yMin: Float, 
      yMax: Float, 
      zBins: Int, 
      zMin: Float, 
      zMax: Float) {
    this(title, new FloatFixedAxis(xBins, xMin, xMax), new FloatFixedAxis(yBins, yMin, yMax), new FloatFixedAxis(zBins, 
      zMin, zMax))
  }

  def allEntries(): Int = nEntry

  def binEntries(indexX: Int, indexY: Int, indexZ: Int): Int = {
    entries(mapX(indexX))(mapY(indexY))(mapZ(indexZ))
  }

  def binError(indexX: Int, indexY: Int, indexZ: Int): Float = {
    Math.sqrt(errors(mapX(indexX))(mapY(indexY))(mapZ(indexZ))).toFloat
  }

  def binHeight(indexX: Int, indexY: Int, indexZ: Int): Float = {
    heights(mapX(indexX))(mapY(indexY))(mapZ(indexZ))
  }

  def equivalentBinEntries(): Float = {
    sumWeight * sumWeight / sumWeightSquared
  }

  def fill(x: Float, y: Float, z: Float) {
    val xBin = mapX(xAxis.coordToIndex(x))
    val yBin = mapY(yAxis.coordToIndex(y))
    val zBin = mapZ(zAxis.coordToIndex(z))
    entries(xBin)(yBin)(zBin) += 1
    heights(xBin)(yBin)(zBin) += 1
    errors(xBin)(yBin)(zBin) += 1
    nEntry += 1
    sumWeight += 1
    sumWeightSquared += 1
    meanX += x
    rmsX += x
    meanY += y
    rmsY += y
    meanZ += z
    rmsZ += z
  }

  def fill(x: Float, 
      y: Float, 
      z: Float, 
      weight: Float) {
    val xBin = mapX(xAxis.coordToIndex(x))
    val yBin = mapY(yAxis.coordToIndex(y))
    val zBin = mapZ(zAxis.coordToIndex(z))
    entries(xBin)(yBin)(zBin) += 1
    heights(xBin)(yBin)(zBin) += weight
    errors(xBin)(yBin)(zBin) += weight * weight
    nEntry += 1
    sumWeight += weight
    sumWeightSquared += weight * weight
    meanX += x * weight
    rmsX += x * weight * weight
    meanY += y * weight
    rmsY += y * weight * weight
    meanZ += z * weight
    rmsZ += z * weight * weight
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
  protected def internalSliceXY(title: String, indexZ1: Int, indexZ2: Int): FloatIHistogram2D = {
    if (indexZ2 < indexZ1) throw new IllegalArgumentException("Invalid bin range")
    val xBins = xAxis.bins() + 2
    val yBins = yAxis.bins() + 2
    val sliceEntries = Array.ofDim[Int](xBins, yBins)
    val sliceHeights = Array.ofDim[Float](xBins, yBins)
    val sliceErrors = Array.ofDim[Float](xBins, yBins)
    for (i <- 0 until xBins; j <- 0 until yBins) {
      var k = indexZ1
      while (k <= indexZ2) {
        sliceEntries(i)(j) += entries(i)(j)(k)
        sliceHeights(i)(j) += heights(i)(j)(k)
        sliceErrors(i)(j) += errors(i)(j)(k)
        k += 1
      }
    }
    val result = new FloatHistogram2D(title, xAxis, yAxis)
    result.setContents(sliceEntries, sliceHeights, sliceErrors)
    result
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
  protected def internalSliceXZ(title: String, indexY1: Int, indexY2: Int): FloatIHistogram2D = {
    if (indexY2 < indexY1) throw new IllegalArgumentException("Invalid bin range")
    val xBins = xAxis.bins() + 2
    val zBins = zAxis.bins() + 2
    val sliceEntries = Array.ofDim[Int](xBins, zBins)
    val sliceHeights = Array.ofDim[Float](xBins, zBins)
    val sliceErrors = Array.ofDim[Float](xBins, zBins)
    for (i <- 0 until xBins) {
      var j = indexY1
      while (j <= indexY2) {
        for (k <- 0 until zBins) {
          sliceEntries(i)(k) += entries(i)(j)(k)
          sliceHeights(i)(k) += heights(i)(j)(k)
          sliceErrors(i)(k) += errors(i)(j)(k)
        }
        j += 1
      }
    }
    val result = new FloatHistogram2D(title, xAxis, zAxis)
    result.setContents(sliceEntries, sliceHeights, sliceErrors)
    result
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
  protected def internalSliceYZ(title: String, indexX1: Int, indexX2: Int): FloatIHistogram2D = {
    if (indexX2 < indexX1) throw new IllegalArgumentException("Invalid bin range")
    val yBins = yAxis.bins() + 2
    val zBins = zAxis.bins() + 2
    val sliceEntries = Array.ofDim[Int](yBins, zBins)
    val sliceHeights = Array.ofDim[Float](yBins, zBins)
    val sliceErrors = Array.ofDim[Float](yBins, zBins)
    var i = indexX1
    while (i <= indexX2) {
      for (j <- 0 until yBins; k <- 0 until zBins) {
        sliceEntries(j)(k) += entries(i)(j)(k)
        sliceHeights(j)(k) += heights(i)(j)(k)
        sliceErrors(j)(k) += errors(i)(j)(k)
      }
      i += 1
    }
    val result = new FloatHistogram2D(title, yAxis, zAxis)
    result.setContents(sliceEntries, sliceHeights, sliceErrors)
    result
  }

  def meanX(): Float = meanX / sumWeight

  def meanY(): Float = meanY / sumWeight

  def meanZ(): Float = meanZ / sumWeight

  def reset() {
    for (i <- 0 until entries.length; j <- 0 until entries(0).length; k <- 0 until entries(0)(0).length) {
      entries(i)(j)(k) = 0
      heights(i)(j)(k) = 0
      errors(i)(j)(k) = 0
    }
    nEntry = 0
    sumWeight = 0
    sumWeightSquared = 0
    meanX = 0
    rmsX = 0
    meanY = 0
    rmsY = 0
    meanZ = 0
    rmsZ = 0
  }

  def rmsX(): Float = {
    Math.sqrt(rmsX / sumWeight - meanX * meanX / sumWeight / sumWeight).toFloat
  }

  def rmsY(): Float = {
    Math.sqrt(rmsY / sumWeight - meanY * meanY / sumWeight / sumWeight).toFloat
  }

  def rmsZ(): Float = {
    Math.sqrt(rmsZ / sumWeight - meanZ * meanZ / sumWeight / sumWeight).toFloat
  }

  def sumAllBinHeights(): Float = sumWeight
}
