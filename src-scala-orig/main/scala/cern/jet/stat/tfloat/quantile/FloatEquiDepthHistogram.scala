package cern.jet.stat.tfloat.quantile

import FloatEquiDepthHistogram._
//remove if not needed
import scala.collection.JavaConversions._

object FloatEquiDepthHistogram {

  /**
   * Not yet commented.
   */
  def test(element: Float) {
    val quantileElements = Array(50.0f, 100.0f, 200.0f, 300.0f, 1400.0f, 1500.0f, 1600.0f, 1700.0f, 1800.0f, 1900.0f, 2000.0f)
    val histo = new FloatEquiDepthHistogram(quantileElements)
    println("elem=" + element + ", phi=" + histo.phi(element))
  }
}

/**
 * Read-only equi-depth histogram for selectivity estimation. Assume you have
 * collected statistics over a data set, among them a one-dimensional equi-depth
 * histogram (quantiles). Then an applications or DBMS might want to estimate
 * the <i>selectivity</i> of some range query <tt>[from,to]</tt>, i.e. the
 * percentage of data set elements contained in the query range. This class does
 * not collect equi-depth histograms but only space efficiently stores already
 * produced histograms and provides operations for selectivity estimation. Uses
 * linear interpolation.
 * <p>
 * This class stores a list <tt>l</tt> of <tt>float</tt> values for which holds:
 * <li>Let <tt>v</tt> be a list of values (sorted ascending) an equi-depth
 * histogram has been computed over.</li>
 * <li>Let <tt>s=l.length</tt>.</li>
 * <li>Let <tt>p=(0, 1/s-1), 2/s-1,..., s-1/s-1=1.0)</tt> be a list of the
 * <tt>s</tt> percentages.</li>
 * <li>Then for each
 * <tt>i=0..s-1: l[i] = e : v.contains(e) && v[0],..., v[p[i]*v.length] &lt;= e</tt>
 * .</li>
 * <li>(In particular: <tt>l[0]=min(v)=v[0]</tt> and
 * <tt>l[s-1]=max(v)=v[s-1]</tt>.)</li>
 *
 * @author wolfgang.hoschek@cern.ch
 * @version 1.0, 09/24/99
 */
@SerialVersionUID(1L)
class FloatEquiDepthHistogram(quantileElements: Array[Float]) extends cern.colt.PersistentObject {

  protected var binBoundaries: Array[Float] = quantileElements

  /**
   * Returns the bin index of the given element. In other words, returns a
   * handle to the range the element falls into.
   *
   * @param element
   *            the element to search for.
   * @throws java.lang.IllegalArgumentException
   *             if the element is not contained in any bin.
   */
  def binOfElement(element: Float): Int = {
    var index = java.util.Arrays.binarySearch(binBoundaries, element)
    if (index >= 0) {
      if (index == binBoundaries.length - 1) index -= 1
    } else {
      index -= -1
      if (index == 0 || index == binBoundaries.length) {
        throw new IllegalArgumentException("Element=" + element + " not contained in any bin.")
      }
      index -= 1
    }
    index
  }

  /**
   * Returns the number of bins. In other words, returns the number of
   * subdomains partitioning the entire value domain.
   */
  def bins(): Int = binBoundaries.length - 1

  /**
   * Returns the end of the range associated with the given bin.
   *
   * @throws ArrayIndexOutOfBoundsException
   *             if <tt>binIndex &lt; 0 || binIndex &gt;= bins()</tt>.
   */
  def endOfBin(binIndex: Int): Float = binBoundaries(binIndex + 1)

  /**
   * Returns the percentage of elements in the range (from,to]. Does linear
   * interpolation.
   *
   * @param from
   *            the start point (exclusive).
   * @param to
   *            the end point (inclusive).
   * @return a number in the closed interval <tt>[0.0,1.0]</tt>.
   */
  def percentFromTo(from: Float, to: Float): Double = phi(to) - phi(from)

  /**
   * Returns how many percent of the elements contained in the receiver are
   * <tt>&lt;= element</tt>. Does linear interpolation.
   *
   * @param element
   *            the element to search for.
   * @return a number in the closed interval <tt>[0.0,1.0]</tt>.
   */
  def phi(element: Float): Double = {
    val size = binBoundaries.length
    if (element <= binBoundaries(0)) return 0.0
    if (element >= binBoundaries(size - 1)) return 1.0
    val binWidth = 1.0 / (size - 1)
    val index = java.util.Arrays.binarySearch(binBoundaries, element)
    if (index >= 0) {
      return binWidth * index
    }
    val insertionPoint = -index - 1
    val from = binBoundaries(insertionPoint - 1)
    val to = binBoundaries(insertionPoint) - from
    val p = (element - from) / to
    binWidth * (p + (insertionPoint - 1))
  }

  /**
   * @deprecated Deprecated. Returns the number of bin boundaries.
   */
  @Deprecated
  def size(): Int = binBoundaries.length

  /**
   * Returns the start of the range associated with the given bin.
   *
   * @throws ArrayIndexOutOfBoundsException
   *             if <tt>binIndex &lt; 0 || binIndex &gt;= bins()</tt>.
   */
  def startOfBin(binIndex: Int): Float = binBoundaries(binIndex)
}
