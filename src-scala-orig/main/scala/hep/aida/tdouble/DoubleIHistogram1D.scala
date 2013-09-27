package hep.aida.tdouble

//remove if not needed
import scala.collection.JavaConversions._

/**
 * A Java interface corresponding to the AIDA 1D Histogram.
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
trait DoubleIHistogram1D extends DoubleIHistogram {

  /**
   * Number of entries in the corresponding bin (ie the number of times fill
   * was called for this bin).
   *
   * @param index
   *            the bin number (0...N-1) or OVERFLOW or UNDERFLOW.
   */
  def binEntries(index: Int): Int

  /**
   * The error on this bin.
   *
   * @param index
   *            the bin number (0...N-1) or OVERFLOW or UNDERFLOW.
   */
  def binError(index: Int): Double

  /**
   * Total height of the corresponding bin (ie the sum of the weights in this
   * bin).
   *
   * @param index
   *            the bin number (0...N-1) or OVERFLOW or UNDERFLOW.
   */
  def binHeight(index: Int): Double

  /**
   * Fill histogram with weight 1.
   */
  def fill(x: Double): Unit

  /**
   * Fill histogram with specified weight.
   */
  def fill(x: Double, weight: Double): Unit

  /**
   * Fill histogram with specified data and weight 1.
   */
  def fill_2D(data: Array[Double], 
      rows: Int, 
      columns: Int, 
      zero: Int, 
      rowStride: Int, 
      columnStride: Int): Unit

  /**
   * Fill histogram with specified data and weights.
   */
  def fill_2D(data: Array[Double], 
      weights: Array[Double], 
      rows: Int, 
      columns: Int, 
      zero: Int, 
      rowStride: Int, 
      columnStride: Int): Unit

  /**
   * Returns the mean of the whole histogram as calculated on filling-time.
   */
  def mean(): Double

  /**
   * Indexes of the in-range bins containing the smallest and largest
   * binHeight(), respectively.
   *
   * @return <tt>{minBin,maxBin}</tt>.
   */
  def minMaxBins(): Array[Int]

  /**
   * Returns the rms of the whole histogram as calculated on filling-time.
   */
  def rms(): Double

  /**
   * Returns the X Axis.
   */
  def xAxis(): DoubleIAxis
}
