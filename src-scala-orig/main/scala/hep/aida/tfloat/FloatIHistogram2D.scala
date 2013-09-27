package hep.aida.tfloat

//remove if not needed
import scala.collection.JavaConversions._

/**
 * A Java interface corresponding to the AIDA 2D Histogram.
 * <p>
 * <b>Note</b> All methods that accept a bin number as an argument will also
 * accept the constants OVERFLOW or UNDERFLOW as the argument, and as a result
 * give the contents of the resulting OVERFLOW or UNDERFLOW bin.
 *
 * @see <a href="http://wwwinfo.cern.ch/asd/lhc++/AIDA/">AIDA</a>
 * @author Pavel Binko, Dino Ferrero Merlino, Wolfgang Hoschek, Tony Johnson,
 *         Andreas Pfeiffer, and others.
 * @version 1.0, 23/03/2000
 */
trait FloatIHistogram2D extends FloatIHistogram {

  /**
   * The number of entries (ie the number of times fill was called for this
   * bin).
   *
   * @param indexX
   *            the x bin number (0...Nx-1) or OVERFLOW or UNDERFLOW.
   * @param indexY
   *            the y bin number (0...Ny-1) or OVERFLOW or UNDERFLOW.
   */
  def binEntries(indexX: Int, indexY: Int): Int

  /**
   * Equivalent to <tt>projectionX().binEntries(indexX)</tt>.
   */
  def binEntriesX(indexX: Int): Int

  /**
   * Equivalent to <tt>projectionY().binEntries(indexY)</tt>.
   */
  def binEntriesY(indexY: Int): Int

  /**
   * The error on this bin.
   *
   * @param indexX
   *            the x bin number (0...Nx-1) or OVERFLOW or UNDERFLOW.
   * @param indexY
   *            the y bin number (0...Ny-1) or OVERFLOW or UNDERFLOW.
   */
  def binError(indexX: Int, indexY: Int): Float

  /**
   * Total height of the corresponding bin (ie the sum of the weights in this
   * bin).
   *
   * @param indexX
   *            the x bin number (0...Nx-1) or OVERFLOW or UNDERFLOW.
   * @param indexY
   *            the y bin number (0...Ny-1) or OVERFLOW or UNDERFLOW.
   */
  def binHeight(indexX: Int, indexY: Int): Float

  /**
   * Equivalent to <tt>projectionX().binHeight(indexX)</tt>.
   */
  def binHeightX(indexX: Int): Float

  /**
   * Equivalent to <tt>projectionY().binHeight(indexY)</tt>.
   */
  def binHeightY(indexY: Int): Float

  /**
   * Fill the histogram with weight 1.
   */
  def fill(x: Float, y: Float): Unit

  /**
   * Fill the histogram with specified weight.
   */
  def fill(x: Float, y: Float, weight: Float): Unit

  /**
   * Returns the mean of the histogram, as calculated on filling-time
   * projected on the X axis.
   */
  def meanX(): Float

  /**
   * Returns the mean of the histogram, as calculated on filling-time
   * projected on the Y axis.
   */
  def meanY(): Float

  /**
   * Indexes of the in-range bins containing the smallest and largest
   * binHeight(), respectively.
   *
   * @return <tt>{minBinX,minBinY, maxBinX,maxBinY}</tt>.
   */
  def minMaxBins(): Array[Int]

  /**
   * Create a projection parallel to the X axis. Equivalent to
   * <tt>sliceX(UNDERFLOW,OVERFLOW)</tt>.
   */
  def projectionX(): FloatIHistogram1D

  /**
   * Create a projection parallel to the Y axis. Equivalent to
   * <tt>sliceY(UNDERFLOW,OVERFLOW)</tt>.
   */
  def projectionY(): FloatIHistogram1D

  /**
   * Returns the rms of the histogram as calculated on filling-time projected
   * on the X axis.
   */
  def rmsX(): Float

  /**
   * Returns the rms of the histogram as calculated on filling-time projected
   * on the Y axis.
   */
  def rmsY(): Float

  /**
   * Slice parallel to the Y axis at bin indexY and one bin wide. Equivalent
   * to <tt>sliceX(indexY,indexY)</tt>.
   */
  def sliceX(indexY: Int): FloatIHistogram1D

  /**
   * Create a slice parallel to the axis X axis, between "indexY1" and
   * "indexY2" (inclusive). The returned IHistogram1D represents an
   * instantaneous snapshot of the histogram at the time the slice was
   * created.
   */
  def sliceX(indexY1: Int, indexY2: Int): FloatIHistogram1D

  /**
   * Slice parallel to the X axis at bin indexX and one bin wide. Equivalent
   * to <tt>sliceY(indexX,indexX)</tt>.
   */
  def sliceY(indexX: Int): FloatIHistogram1D

  /**
   * Create a slice parallel to the axis Y axis, between "indexX1" and
   * "indexX2" (inclusive) The returned IHistogram1D represents an
   * instantaneous snapshot of the histogram at the time the slice was
   * created.
   */
  def sliceY(indexX1: Int, indexX2: Int): FloatIHistogram1D

  /**
   * Return the X axis.
   */
  def xAxis(): FloatIAxis

  /**
   * Return the Y axis.
   */
  def yAxis(): FloatIAxis
}
