package hep.aida.tfloat.ref

import hep.aida.tfloat.FloatIAxis
import hep.aida.tfloat.FloatIHistogram1D
import hep.aida.tfloat.FloatIHistogram2D
//remove if not needed
import scala.collection.JavaConversions._

/**
 * A reference implementation of hep.aida.IHistogram2D. The goal is to provide a
 * clear implementation rather than the most efficient implementation. However,
 * performance seems fine - filling 6 * 10^5 points/sec, both using FixedAxis or
 * VariableAxis.
 *
 * @author Wolfgang Hoschek, Tony Johnson, and others.
 * @version 1.0, 23/03/2000
 */
@SerialVersionUID(1L)
class FloatHistogram2D(title: String, xAxis: FloatIAxis, yAxis: FloatIAxis) extends FloatAbstractHistogram2D(title) with FloatIHistogram2D {

  private var heights: Array[Array[Float]] = new Array[Array[Float]](xBins + 2, yBins + 2)

  private var errors: Array[Array[Float]] = new Array[Array[Float]](xBins + 2, yBins + 2)

  private var entries: Array[Array[Int]] = new Array[Array[Int]](xBins + 2, yBins + 2)

  private var nEntry: Int = _

  private var sumWeight: Float = _

  private var sumWeightSquared: Float = _

  private var meanX: Float = _

  private var rmsX: Float = _

  private var meanY: Float = _

  private var rmsY: Float = _

  this.xAxis = xAxis

  this.yAxis = yAxis

  val xBins = xAxis.bins()

  val yBins = yAxis.bins()

  /**
   * Creates a variable-width histogram. Example:
   * <tt>xEdges = (0.2, 1.0, 5.0, 6.0), yEdges = (-5, 0, 7)</tt> yields 3*2
   * in-range bins.
   *
   * @param title
   *            The histogram title.
   * @param xEdges
   *            the bin boundaries the x-axis shall have; must be sorted
   *            ascending and must not contain multiple identical elements.
   * @param yEdges
   *            the bin boundaries the y-axis shall have; must be sorted
   *            ascending and must not contain multiple identical elements.
   * @throws IllegalArgumentException
   *             if <tt>xEdges.length < 1 || yEdges.length < 1</tt>.
   */
  def this(title: String, xEdges: Array[Float], yEdges: Array[Float]) {
    this(title, new FloatVariableAxis(xEdges), new FloatVariableAxis(yEdges))
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
   */
  def this(title: String, 
      xBins: Int, 
      xMin: Float, 
      xMax: Float, 
      yBins: Int, 
      yMin: Float, 
      yMax: Float) {
    this(title, new FloatFixedAxis(xBins, xMin, xMax), new FloatFixedAxis(yBins, yMin, yMax))
  }

  def allEntries(): Int = nEntry

  def binEntries(indexX: Int, indexY: Int): Int = entries(mapX(indexX))(mapY(indexY))

  def binError(indexX: Int, indexY: Int): Float = {
    Math.sqrt(errors(mapX(indexX))(mapY(indexY))).toFloat
  }

  def binHeight(indexX: Int, indexY: Int): Float = heights(mapX(indexX))(mapY(indexY))

  def equivalentBinEntries(): Float = {
    sumWeight * sumWeight / sumWeightSquared
  }

  def fill(x: Float, y: Float) {
    val xBin = mapX(xAxis.coordToIndex(x))
    val yBin = mapY(yAxis.coordToIndex(y))
    entries(xBin)(yBin) += 1
    heights(xBin)(yBin) += 1
    errors(xBin)(yBin) += 1
    nEntry += 1
    sumWeight += 1
    sumWeightSquared += 1
    meanX += x
    rmsX += x
    meanY += y
    rmsY += y
  }

  def fill(x: Float, y: Float, weight: Float) {
    val xBin = mapX(xAxis.coordToIndex(x))
    val yBin = mapY(yAxis.coordToIndex(y))
    entries(xBin)(yBin) += 1
    heights(xBin)(yBin) += weight
    errors(xBin)(yBin) += weight * weight
    nEntry += 1
    sumWeight += weight
    sumWeightSquared += weight * weight
    meanX += x * weight
    rmsX += x * weight * weight
    meanY += y * weight
    rmsY += y * weight * weight
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
  protected def internalSliceX(title: String, indexY1: Int, indexY2: Int): FloatIHistogram1D = {
    if (indexY2 < indexY1) throw new IllegalArgumentException("Invalid bin range")
    val sliceBins = xAxis.bins() + 2
    val sliceEntries = Array.ofDim[Int](sliceBins)
    val sliceHeights = Array.ofDim[Float](sliceBins)
    val sliceErrors = Array.ofDim[Float](sliceBins)
    for (i <- 0 until sliceBins) {
      var j = indexY1
      while (j <= indexY2) {
        sliceEntries(i) += entries(i)(j)
        sliceHeights(i) += heights(i)(j)
        sliceErrors(i) += errors(i)(j)
        j += 1
      }
    }
    val result = new FloatHistogram1D(title, xAxis)
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
  protected def internalSliceY(title: String, indexX1: Int, indexX2: Int): FloatIHistogram1D = {
    if (indexX2 < indexX1) throw new IllegalArgumentException("Invalid bin range")
    val sliceBins = yAxis.bins() + 2
    val sliceEntries = Array.ofDim[Int](sliceBins)
    val sliceHeights = Array.ofDim[Float](sliceBins)
    val sliceErrors = Array.ofDim[Float](sliceBins)
    var i = indexX1
    while (i <= indexX2) {
      for (j <- 0 until sliceBins) {
        sliceEntries(j) += entries(i)(j)
        sliceHeights(j) += heights(i)(j)
        sliceErrors(j) += errors(i)(j)
      }
      i += 1
    }
    val result = new FloatHistogram1D(title, yAxis)
    result.setContents(sliceEntries, sliceHeights, sliceErrors)
    result
  }

  def meanX(): Float = meanX / sumWeight

  def meanY(): Float = meanY / sumWeight

  def reset() {
    for (i <- 0 until entries.length; j <- 0 until entries(0).length) {
      entries(i)(j) = 0
      heights(i)(j) = 0
      errors(i)(j) = 0
    }
    nEntry = 0
    sumWeight = 0
    sumWeightSquared = 0
    meanX = 0
    rmsX = 0
    meanY = 0
    rmsY = 0
  }

  def rmsX(): Float = {
    Math.sqrt(rmsX / sumWeight - meanX * meanX / sumWeight / sumWeight).toFloat
  }

  def rmsY(): Float = {
    Math.sqrt(rmsY / sumWeight - meanY * meanY / sumWeight / sumWeight).toFloat
  }

  /**
   * Used internally for creating slices and projections
   */
  def setContents(entries: Array[Array[Int]], heights: Array[Array[Float]], errors: Array[Array[Float]]) {
    this.entries = entries
    this.heights = heights
    this.errors = errors
    for (i <- 0 until entries.length; j <- 0 until entries(0).length) {
      nEntry += entries(i)(j)
      sumWeight += heights(i)(j)
    }
    sumWeightSquared = Float.NaN
    meanX = Float.NaN
    rmsX = Float.NaN
    meanY = Float.NaN
    rmsY = Float.NaN
  }

  def sumAllBinHeights(): Float = sumWeight
}
