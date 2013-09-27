package hep.aida.tdouble

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
trait DoubleIAxis extends java.io.Serializable {

  /**
   * Centre of the bin specified.
   *
   * @param index
   *            Bin number (0...bins()-1) or OVERFLOW or UNDERFLOW.
   */
  def binCentre(index: Int): Double

  /**
   * Lower edge of the specified bin.
   *
   * @param index
   *            Bin number (0...bins()-1) or OVERFLOW or UNDERFLOW.
   * @return the lower edge of the bin; for the underflow bin this is
   *         <tt>Double.NEGATIVE_INFINITY</tt>.
   */
  def binLowerEdge(index: Int): Double

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
   *         <tt>Double.POSITIVE_INFINITY</tt>.
   */
  def binUpperEdge(index: Int): Double

  /**
   * Width of the bin specified.
   *
   * @param index
   *            Bin number (0...bins()-1) or OVERFLOW or UNDERFLOW.
   */
  def binWidth(index: Int): Double

  /**
   * Converts a coordinate on the axis to a bin number. If the coordinate is <
   * lowerEdge returns UNDERFLOW, and if the coordinate is >= upperEdge
   * returns OVERFLOW.
   */
  def coordToIndex(coord: Double): Int

  /**
   * Lower axis edge.
   */
  def lowerEdge(): Double

  /**
   * Upper axis edge.
   */
  def upperEdge(): Double
}
