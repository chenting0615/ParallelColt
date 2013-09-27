package hep.aida.tfloat

//remove if not needed
import scala.collection.JavaConversions._

/**
 * An IAxis represents a binned histogram axis. A 1D Histogram would have one
 * Axis representing the X axis, while a 2D Histogram would have two axes
 * representing the X and Y Axis.
 *
 * @author Pavel Binko, Dino Ferrero Merlino, Wolfgang Hoschek, Tony Johnson,
 *         Andreas Pfeiffer, and others.
 * @version 1.0, 23/03/2000
 */
@SerialVersionUID(1020)
trait FloatIAxis extends java.io.Serializable {

  /**
   * Centre of the bin specified.
   *
   * @param index
   *            Bin number (0...bins()-1) or OVERFLOW or UNDERFLOW.
   */
  def binCentre(index: Int): Float

  /**
   * Lower edge of the specified bin.
   *
   * @param index
   *            Bin number (0...bins()-1) or OVERFLOW or UNDERFLOW.
   * @return the lower edge of the bin; for the underflow bin this is
   *         <tt>Float.NEGATIVE_INFINITY</tt>.
   */
  def binLowerEdge(index: Int): Float

  /**
   * The number of bins (excluding underflow and overflow) on the axis.
   */
  def bins(): Int

  /**
   * Upper edge of the specified bin.
   *
   * @param index
   *            Bin number (0...bins()-1) or OVERFLOW or UNDERFLOW.
   * @return the upper edge of the bin; for the overflow bin this is
   *         <tt>Float.POSITIVE_INFINITY</tt>.
   */
  def binUpperEdge(index: Int): Float

  /**
   * Width of the bin specified.
   *
   * @param index
   *            Bin number (0...bins()-1) or OVERFLOW or UNDERFLOW.
   */
  def binWidth(index: Int): Float

  /**
   * Converts a coordinate on the axis to a bin number. If the coordinate is <
   * lowerEdge returns UNDERFLOW, and if the coordinate is >= upperEdge
   * returns OVERFLOW.
   */
  def coordToIndex(coord: Float): Int

  /**
   * Lower axis edge.
   */
  def lowerEdge(): Float

  /**
   * Upper axis edge.
   */
  def upperEdge(): Float
}
