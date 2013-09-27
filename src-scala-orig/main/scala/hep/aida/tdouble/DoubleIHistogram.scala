package hep.aida.tdouble

import DoubleIHistogram._
//remove if not needed
import scala.collection.JavaConversions._

object DoubleIHistogram {

  /**
   * Constant specifying the overflow bin (can be passed to any method
   * expecting a bin number).
   */
  val OVERFLOW = -1

  /**
   * Constant specifying the underflow bin (can be passed to any method
   * expecting a bin number).
   */
  val UNDERFLOW = -2
}

/**
 * A common base interface for IHistogram1D, IHistogram2D and IHistogram3D.
 *
 * @author Pavel Binko, Dino Ferrero Merlino, Wolfgang Hoschek, Tony Johnson,
 *         Andreas Pfeiffer, and others.
 * @version 1.0, 23/03/2000
 */
@SerialVersionUID(1020)
trait DoubleIHistogram extends java.io.Serializable {

  /**
   * Number of all entries in all (both in-range and under/overflow) bins in
   * the histogram.
   */
  def allEntries(): Int

  /**
   * Returns 1 for one-dimensional histograms, 2 for two-dimensional
   * histograms, and so on.
   */
  def dimensions(): Int

  /**
   * Number of in-range entries in the histogram.
   */
  def entries(): Int

  /**
   * Number of equivalent entries.
   *
   * @return <tt>SUM[ weight ] ^ 2 / SUM[ weight^2 ]</tt>.
   */
  def equivalentBinEntries(): Double

  /**
   * Number of under and overflow entries in the histogram.
   */
  def extraEntries(): Int

  /**
   * Reset contents; as if just constructed.
   */
  def reset(): Unit

  /**
   * Sum of all (both in-range and under/overflow) bin heights in the
   * histogram.
   */
  def sumAllBinHeights(): Double

  /**
   * Sum of in-range bin heights in the histogram.
   */
  def sumBinHeights(): Double

  /**
   * Sum of under/overflow bin heights in the histogram.
   */
  def sumExtraBinHeights(): Double

  /**
   * Title of the histogram (will be set only in the constructor).
   */
  def title(): String
}
