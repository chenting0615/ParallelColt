package hep.aida.tfloat

//remove if not needed
import scala.collection.JavaConversions._

/**
 * A Java interface corresponding to the AIDA 3D Histogram.
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
trait FloatIHistogram3D extends FloatIHistogram {

  /**
   * The number of entries (ie the number of times fill was called for this
   * bin).
   *
   * @param indexX
   *            the x bin number (0...Nx-1) or OVERFLOW or UNDERFLOW.
   * @param indexY
   *            the y bin number (0...Ny-1) or OVERFLOW or UNDERFLOW.
   * @param indexZ
   *            the z bin number (0...Nz-1) or OVERFLOW or UNDERFLOW.
   */
  def binEntries(indexX: Int, indexY: Int, indexZ: Int): Int

  /**
   * The error on this bin.
   *
   * @param indexX
   *            the x bin number (0...Nx-1) or OVERFLOW or UNDERFLOW.
   * @param indexY
   *            the y bin number (0...Ny-1) or OVERFLOW or UNDERFLOW.
   * @param indexZ
   *            the z bin number (0...Nz-1) or OVERFLOW or UNDERFLOW.
   */
  def binError(indexX: Int, indexY: Int, indexZ: Int): Float

  /**
   * Total height of the corresponding bin (ie the sum of the weights in this
   * bin).
   *
   * @param indexX
   *            the x bin number (0...Nx-1) or OVERFLOW or UNDERFLOW.
   * @param indexY
   *            the y bin number (0...Ny-1) or OVERFLOW or UNDERFLOW.
   * @param indexZ
   *            the z bin number (0...Nz-1) or OVERFLOW or UNDERFLOW.
   */
  def binHeight(indexX: Int, indexY: Int, indexZ: Int): Float

  /**
   * Fill the histogram with weight 1; equivalent to <tt>fill(x,y,z,1)</tt>..
   */
  def fill(x: Float, y: Float, z: Float): Unit

  /**
   * Fill the histogram with specified weight.
   */
  def fill(x: Float, 
      y: Float, 
      z: Float, 
      weight: Float): Unit

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
   * Returns the mean of the histogram, as calculated on filling-time
   * projected on the Z axis.
   */
  def meanZ(): Float

  /**
   * Indexes of the in-range bins containing the smallest and largest
   * binHeight(), respectively.
   *
   * @return <tt>{minBinX,minBinY,minBinZ, maxBinX,maxBinY,maxBinZ}</tt>.
   */
  def minMaxBins(): Array[Int]

  /**
   * Create a projection parallel to the XY plane. Equivalent to
   * <tt>sliceXY(UNDERFLOW,OVERFLOW)</tt>.
   */
  def projectionXY(): FloatIHistogram2D

  /**
   * Create a projection parallel to the XZ plane. Equivalent to
   * <tt>sliceXZ(UNDERFLOW,OVERFLOW)</tt>.
   */
  def projectionXZ(): FloatIHistogram2D

  /**
   * Create a projection parallel to the YZ plane. Equivalent to
   * <tt>sliceYZ(UNDERFLOW,OVERFLOW)</tt>.
   */
  def projectionYZ(): FloatIHistogram2D

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
   * Returns the rms of the histogram as calculated on filling-time projected
   * on the Z axis.
   */
  def rmsZ(): Float

  /**
   * Create a slice parallel to the XY plane at bin indexZ and one bin wide.
   * Equivalent to <tt>sliceXY(indexZ,indexZ)</tt>.
   */
  def sliceXY(indexZ: Int): FloatIHistogram2D

  /**
   * Create a slice parallel to the XY plane, between "indexZ1" and "indexZ2"
   * (inclusive). The returned IHistogram2D represents an instantaneous
   * snapshot of the histogram at the time the slice was created.
   */
  def sliceXY(indexZ1: Int, indexZ2: Int): FloatIHistogram2D

  /**
   * Create a slice parallel to the XZ plane at bin indexY and one bin wide.
   * Equivalent to <tt>sliceXZ(indexY,indexY)</tt>.
   */
  def sliceXZ(indexY: Int): FloatIHistogram2D

  /**
   * Create a slice parallel to the XZ plane, between "indexY1" and "indexY2"
   * (inclusive). The returned IHistogram2D represents an instantaneous
   * snapshot of the histogram at the time the slice was created.
   */
  def sliceXZ(indexY1: Int, indexY2: Int): FloatIHistogram2D

  /**
   * Create a slice parallel to the YZ plane at bin indexX and one bin wide.
   * Equivalent to <tt>sliceYZ(indexX,indexX)</tt>.
   */
  def sliceYZ(indexX: Int): FloatIHistogram2D

  /**
   * Create a slice parallel to the YZ plane, between "indexX1" and "indexX2"
   * (inclusive). The returned IHistogram2D represents an instantaneous
   * snapshot of the histogram at the time the slice was created.
   */
  def sliceYZ(indexX1: Int, indexX2: Int): FloatIHistogram2D

  /**
   * Return the X axis.
   */
  def xAxis(): FloatIAxis

  /**
   * Return the Y axis.
   */
  def yAxis(): FloatIAxis

  /**
   * Return the Z axis.
   */
  def zAxis(): FloatIAxis
}
