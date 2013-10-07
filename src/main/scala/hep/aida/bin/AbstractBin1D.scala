package hep.aida.bin

import cern.jet.stat.tdouble.DoubleDescriptive

/**
 * Abstract base class for all 1-dimensional bins consumes <tt>double</tt>
 * elements. First see the <a href="package-summary.html">package summary</a>
 * and javadoc <a href="package-tree.html">tree view</a> to get the broad
 * picture.
 * <p>
 * This class is fully thread safe (all public methods are synchronized). Thus,
 * you can have one or more threads adding to the bin as well as one or more
 * threads reading and viewing the statistics of the bin <i>while it is
 * filled</i>. For high performance, add data in large chunks (buffers) via
 * method <tt>addAllOf</tt> rather than piecewise via method <tt>add</tt>.
 *
 * @author wolfgang.hoschek@cern.ch
 * @version 0.9, 03-Jul-99
 */
@SerialVersionUID(1L)
abstract class AbstractBin1D[@specialized T: Numeric] extends AbstractBin[T] {

  /**
   * Adds the specified element to the receiver.
   *
   * @param element
   *            element to be appended.
   */
  def add(element: T): Unit

  /**
   * Adds all values of the specified list to the receiver.
   *
   * @param list
   *            the list of which all values shall be added.
   */
  def addAllOf(list: Traversable[T]) {
    list.foreach(v => add(v))
  }

  /**
   * Adds the part of the specified list between indexes <tt>from</tt>
   * (inclusive) and <tt>to</tt> (inclusive) to the receiver. You may want to
   * override this method for performance reasons.
   *
   * @param list
   *            the list of which elements shall be added.
   * @param from
   *            the index of the first element to be added (inclusive).
   * @param to
   *            the index of the last element to be added (inclusive).
   * @throws IndexOutOfBoundsException
   *             if
   *             <tt>list.size()&gt;0 && (from&lt;0 || from&gt;to || to&gt;=list.size())</tt>
   *             .
   */
  def addAllOfFromTo(list: IndexedSeq[T], from: Int, to: Int) {
    for(idx <- from until to) add(list(idx))
  }

  /**
   * Computes the deviations from the receiver's measures to another bin's
   * measures.
   *
   * @param other
   *            the other bin to compare with
   * @return a summary of the deviations.
   */
  def compareWith(other: AbstractBin1D[T]): String = {
    val buf = new StringBuffer()
    buf.append("\nDifferences [percent]")
    buf.append("\nSize: " + relError(size, other.size) + " %")
    buf.append("\nSum: " + relError(sum, other.sum) + " %")
    buf.append("\nSumOfSquares: " + relError(sumOfSquares, other.sumOfSquares) +
      " %")
    buf.append("\nMin: " + relError(min.toString.toDouble, other.min.toString.toDouble) + " %")
    buf.append("\nMax: " + relError(max.toString.toDouble, other.max.toString.toDouble) + " %")
    buf.append("\nMean: " + relError(mean, other.mean) + " %")
    buf.append("\nRMS: " + relError(rms, other.rms) + " %")
    buf.append("\nVariance: " + relError(variance, other.variance) +
      " %")
    buf.append("\nStandard deviation: " +
      relError(standardDeviation, other.standardDeviation) +
      " %")
    buf.append("\nStandard error: " + relError(standardError, other.standardError) +
      " %")
    buf.append("\n")
    buf.toString
  }

  /**
   * Returns whether two bins are equal; They are equal if the other object is
   * of the same class or a subclass of this class and both have the same
   * size, minimum, maximum, sum and sumOfSquares.
   */
  override def equals(obj: Any): Boolean = {
    if (obj == null) return false
    if (! obj.isInstanceOf[AbstractBin1D[T]]) return false
    val other = obj.asInstanceOf[AbstractBin1D[T]]
    if (other eq this) return true
    size == other.size && min == other.min && max == other.max && sum == other.sum && sumOfSquares == other.sumOfSquares
  }

  /**
   * Returns the maximum.
   */
  def max: Double

  /**
   * Returns the arithmetic mean, which is <tt>Sum( x[i] ) / size()</tt>.
   */
  def mean: Double = sum / size

  /**
   * Returns the minimum.
   */
  def min: Double

  /**
   * Computes the relative error (in percent) from one measure to another.
   */
  protected def relError(measure1: Double, measure2: Double): Double = 100 * (1 - measure1 / measure2)

  /**
   * Returns the rms (Root Mean Square), which is
   * <tt>Math.sqrt( Sum( x[i]*x[i] ) / size() )</tt>.
   */
  def rms: Double = {
    DoubleDescriptive.rms(size, sumOfSquares)
  }

  /**
   * Returns the sample standard deviation, which is
   * <tt>Math.sqrt(variance())</tt>.
   */
  def standardDeviation: Double = {
    Math.sqrt(variance)
  }

  /**
   * Returns the sample standard error, which is
   * <tt>Math.sqrt(variance() / size())</tt>
   */
  def standardError: Double = {
    DoubleDescriptive.standardError(size, variance)
  }

  /**
   * Returns the sum of all elements, which is <tt>Sum( x[i] )</tt>.
   */
  def sum: Double

  /**
   * Returns the sum of squares, which is <tt>Sum( x[i] * x[i] )</tt>.
   */
  def sumOfSquares: Double

  /**
   * Returns a String representation of the receiver.
   */
  override def toString: String = {
    synchronized {
      val buf = new StringBuffer()
      buf.append(getClass.getName)
      buf.append("\n-------------")
      buf.append("\nSize: " + size)
      buf.append("\nSum: " + sum)
      buf.append("\nSumOfSquares: " + sumOfSquares)
      buf.append("\nMin: " + min)
      buf.append("\nMax: " + max)
      buf.append("\nMean: " + mean)
      buf.append("\nRMS: " + rms)
      buf.append("\nVariance: " + variance)
      buf.append("\nStandard deviation: " + standardDeviation)
      buf.append("\nStandard error: " + standardError)
      buf.append("\n")
      buf.toString
    }
  }

  /**
   * Returns the sample variance, which is
   * <tt>Sum( (x[i]-mean())<sup>2</sup> )  /  (size()-1)</tt>.
   */
  def variance: Double = {
    DoubleDescriptive.sampleVariance(size, sum, sumOfSquares)
  }
}
