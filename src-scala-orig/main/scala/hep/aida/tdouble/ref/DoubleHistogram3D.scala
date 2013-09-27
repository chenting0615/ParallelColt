package hep.aida.tdouble.ref

import hep.aida.tdouble.DoubleIAxis
import hep.aida.tdouble.DoubleIHistogram2D
import hep.aida.tdouble.DoubleIHistogram3D
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
class DoubleHistogram3D(title: String, 
    xAxis: DoubleIAxis, 
    yAxis: DoubleIAxis, 
    zAxis: DoubleIAxis) extends DoubleAbstractHistogram3D(title) with DoubleIHistogram3D {

  private var heights: Array[Array[Array[Double]]] = new Array[Array[Array[Double]]](xBins + 2, yBins + 2, zBins + 2)

  private var errors: Array[Array[Array[Double]]] = new Array[Array[Array[Double]]](xBins + 2, yBins + 2, zBins + 2)

  private var entries: Array[Array[Array[Int]]] = new Array[Array[Array[Int]]](xBins + 2, yBins + 2, zBins + 2)

  private var nEntry: Int = _

  private var sumWeight: Double = _

  private var sumWeightSquared: Double = _

  private var meanX: Double = _

  private var rmsX: Double = _

  private var meanY: Double = _

  private var rmsY: Double = _

  private var meanZ: Double = _

  private var rmsZ: Double = _

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
      xEdges: Array[Double], 
      yEdges: Array[Double], 
      zEdges: Array[Double]) {
    this(title, new DoubleVariableAxis(xEdges), new DoubleVariableAxis(yEdges), new DoubleVariableAxis(zEdges))
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
      xMin: Double, 
      xMax: Double, 
      yBins: Int, 
      yMin: Double, 
      yMax: Double, 
      zBins: Int, 
      zMin: Double, 
      zMax: Double) {
    this(title, new DoubleFixedAxis(xBins, xMin, xMax), new DoubleFixedAxis(yBins, yMin, yMax), new DoubleFixedAxis(zBins, 
      zMin, zMax))
  }

  def allEntries(): Int = nEntry

  def binEntries(indexX: Int, indexY: Int, indexZ: Int): Int = {
    entries(mapX(indexX))(mapY(indexY))(mapZ(indexZ))
  }

  def binError(indexX: Int, indexY: Int, indexZ: Int): Double = {
    Math.sqrt(errors(mapX(indexX))(mapY(indexY))(mapZ(indexZ)))
  }

  def binHeight(indexX: Int, indexY: Int, indexZ: Int): Double = {
    heights(mapX(indexX))(mapY(indexY))(mapZ(indexZ))
  }

  def equivalentBinEntries(): Double = {
    sumWeight * sumWeight / sumWeightSquared
  }

  def fill(x: Double, y: Double, z: Double) {
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

  def fill(x: Double, 
      y: Double, 
      z: Double, 
      weight: Double) {
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
  protected def internalSliceXY(title: String, indexZ1: Int, indexZ2: Int): DoubleIHistogram2D = {
    if (indexZ2 < indexZ1) throw new IllegalArgumentException("Invalid bin range")
    val xBins = xAxis.bins() + 2
    val yBins = yAxis.bins() + 2
    val sliceEntries = Array.ofDim[Int](xBins, yBins)
    val sliceHeights = Array.ofDim[Double](xBins, yBins)
    val sliceErrors = Array.ofDim[Double](xBins, yBins)
    for (i <- 0 until xBins; j <- 0 until yBins) {
      var k = indexZ1
      while (k <= indexZ2) {
        sliceEntries(i)(j) += entries(i)(j)(k)
        sliceHeights(i)(j) += heights(i)(j)(k)
        sliceErrors(i)(j) += errors(i)(j)(k)
        k += 1
      }
    }
    val result = new DoubleHistogram2D(title, xAxis, yAxis)
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
  protected def internalSliceXZ(title: String, indexY1: Int, indexY2: Int): DoubleIHistogram2D = {
    if (indexY2 < indexY1) throw new IllegalArgumentException("Invalid bin range")
    val xBins = xAxis.bins() + 2
    val zBins = zAxis.bins() + 2
    val sliceEntries = Array.ofDim[Int](xBins, zBins)
    val sliceHeights = Array.ofDim[Double](xBins, zBins)
    val sliceErrors = Array.ofDim[Double](xBins, zBins)
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
    val result = new DoubleHistogram2D(title, xAxis, zAxis)
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
  protected def internalSliceYZ(title: String, indexX1: Int, indexX2: Int): DoubleIHistogram2D = {
    if (indexX2 < indexX1) throw new IllegalArgumentException("Invalid bin range")
    val yBins = yAxis.bins() + 2
    val zBins = zAxis.bins() + 2
    val sliceEntries = Array.ofDim[Int](yBins, zBins)
    val sliceHeights = Array.ofDim[Double](yBins, zBins)
    val sliceErrors = Array.ofDim[Double](yBins, zBins)
    var i = indexX1
    while (i <= indexX2) {
      for (j <- 0 until yBins; k <- 0 until zBins) {
        sliceEntries(j)(k) += entries(i)(j)(k)
        sliceHeights(j)(k) += heights(i)(j)(k)
        sliceErrors(j)(k) += errors(i)(j)(k)
      }
      i += 1
    }
    val result = new DoubleHistogram2D(title, yAxis, zAxis)
    result.setContents(sliceEntries, sliceHeights, sliceErrors)
    result
  }

  def meanX(): Double = meanX / sumWeight

  def meanY(): Double = meanY / sumWeight

  def meanZ(): Double = meanZ / sumWeight

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

  def rmsX(): Double = {
    Math.sqrt(rmsX / sumWeight - meanX * meanX / sumWeight / sumWeight)
  }

  def rmsY(): Double = {
    Math.sqrt(rmsY / sumWeight - meanY * meanY / sumWeight / sumWeight)
  }

  def rmsZ(): Double = {
    Math.sqrt(rmsZ / sumWeight - meanZ * meanZ / sumWeight / sumWeight)
  }

  def sumAllBinHeights(): Double = sumWeight
}
