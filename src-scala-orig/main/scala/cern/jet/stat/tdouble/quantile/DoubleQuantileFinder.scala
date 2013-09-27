package cern.jet.stat.tdouble.quantile

import cern.colt.list.tdouble.DoubleArrayList
//remove if not needed
import scala.collection.JavaConversions._

/**
 * The interface shared by all quantile finders, no matter if they are exact or
 * approximate. It is usually completely sufficient to operate on this interface
 * only. Also see {@link hep.aida.tdouble.bin.QuantileDoubleBin1D},
 * demonstrating how this package can be used.
 */
trait DoubleQuantileFinder extends java.io.Serializable {

  /**
   * Adds a value to the receiver.
   *
   * @param value
   *            the value to add.
   */
  def add(value: Double): Unit

  /**
   * Adds all values of the specified list to the receiver.
   *
   * @param values
   *            the list of which all values shall be added.
   */
  def addAllOf(values: cern.colt.list.tdouble.DoubleArrayList): Unit

  /**
   * Adds the part of the specified list between indexes <tt>from</tt>
   * (inclusive) and <tt>to</tt> (inclusive) to the receiver.
   *
   * @param values
   *            the list of which elements shall be added.
   * @param from
   *            the index of the first element to be added (inclusive).
   * @param to
   *            the index of the last element to be added (inclusive).
   */
  def addAllOfFromTo(values: DoubleArrayList, from: Int, to: Int): Unit

  /**
   * Removes all elements from the receiver. The receiver will be empty after
   * this call returns, and its memory requirements will be close to zero.
   */
  def clear(): Unit

  /**
   * Returns a deep copy of the receiver.
   *
   * @return a deep copy of the receiver.
   */
  def clone(): AnyRef

  /**
   * Applies a procedure to each element of the receiver, if any. Iterates
   * over the receiver in no particular order.
   *
   * @param procedure
   *            the procedure to be applied. Stops iteration if the procedure
   *            returns <tt>false</tt>, otherwise continues.
   * @return <tt>false</tt> if the procedure stopped before all elements where
   *         iterated over, <tt>true</tt> otherwise.
   */
  def forEach(procedure: cern.colt.function.tdouble.Procedure1): Boolean

  /**
   * Returns the number of elements currently needed to store all contained
   * elements. This number usually differs from the results of method
   * <tt>size()</tt>, according to the underlying datastructure.
   */
  def memory(): Long

  /**
   * Returns how many percent of the elements contained in the receiver are
   * <tt>&lt;= element</tt>. Does linear interpolation if the element is not
   * contained but lies in between two contained elements.
   *
   * Writing a wrapper is a good idea if you can think of better ways of doing
   * interpolation. Same if you want to keep min,max and other such measures.
   *
   * @param element
   *            the element to search for.
   * @return the percentage <tt>p</tt> of elements <tt>&lt;= element</tt> (
   *         <tt>0.0 &lt;= p &lt;=1.0)</tt>.
   */
  def phi(element: Double): Double

  /**
   * Computes the specified quantile elements over the values previously
   * added.
   *
   * @param phis
   *            the quantiles for which elements are to be computed. Each phi
   *            must be in the interval [0.0,1.0]. <tt>phis</tt> must be
   *            sorted ascending.
   * @return the quantile elements.
   */
  def quantileElements(phis: DoubleArrayList): DoubleArrayList

  /**
   * Returns the number of elements currently contained in the receiver
   * (identical to the number of values added so far).
   */
  def size(): Long

  /**
   * Returns the number of elements currently needed to store all contained
   * elements. This number usually differs from the results of method
   * <tt>size()</tt>, according to the underlying datastructure.
   */
  def totalMemory(): Long
}
