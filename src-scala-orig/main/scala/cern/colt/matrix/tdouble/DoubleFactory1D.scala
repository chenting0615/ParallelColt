package cern.colt.matrix.tdouble

import cern.colt.matrix.tdouble.impl.DenseMatrix1D
import cern.colt.matrix.tdouble.impl.SparseDoubleMatrix1D
import cern.jet.math.tdouble.DoubleFunctions
import DoubleFactory1D._
//remove if not needed
import scala.collection.JavaConversions._

object DoubleFactory1D {

  /**
   * A factory producing dense matrices.
   */
  val dense = new DoubleFactory1D()

  /**
   * A factory producing sparse matrices.
   */
  val sparse = new DoubleFactory1D()
}

/**
 * Factory for convenient construction of 1-d matrices holding <tt>double</tt>
 * cells. Use idioms like <tt>DoubleFactory1D.dense.make(1000)</tt> to construct
 * dense matrices, <tt>DoubleFactory1D.sparse.make(1000)</tt> to construct
 * sparse matrices.
 *
 * If the factory is used frequently it might be useful to streamline the
 * notation. For example by aliasing:
 * <table>
 * <td class="PRE">
 *
 * <pre>
 *  DoubleFactory1D F = DoubleFactory1D.dense;
 *  F.make(1000);
 *  F.descending(10);
 *  F.random(3);
 *  ...
 * </pre>
 *
 * </td>
 * </table>
 *
 * @author wolfgang.hoschek@cern.ch
 * @version 1.0, 09/24/99
 */
@SerialVersionUID(1L)
class DoubleFactory1D protected () extends cern.colt.PersistentObject {

  /**
   * C = A||B; Constructs a new matrix which is the concatenation of two other
   * matrices. Example: <tt>0 1</tt> append <tt>3 4</tt> --> <tt>0 1 3 4</tt>.
   */
  def append(A: StrideMatrix1D, B: StrideMatrix1D): StrideMatrix1D = {
    val matrix = make((A.size + B.size).toInt)
    matrix.viewPart(0, A.size.toInt).assign(A)
    matrix.viewPart(A.size.toInt, B.size.toInt).assign(B)
    matrix
  }

  /**
   * Constructs a matrix with cells having ascending values. For debugging
   * purposes. Example: <tt>0 1 2</tt>
   */
  def ascending(size: Int): StrideMatrix1D = {
    descending(size).assign(DoubleFunctions.chain(DoubleFunctions.neg, DoubleFunctions.minus(size)))
  }

  /**
   * Constructs a matrix with cells having descending values. For debugging
   * purposes. Example: <tt>2 1 0</tt>
   */
  def descending(size: Int): StrideMatrix1D = {
    val matrix = make(size)
    var v = 0
    var i = size
    while (i >= 0) {
      matrix.setQuick(i, v += 1)
    }
    matrix
  }

  /**
   * Constructs a matrix from the values of the given list. The values are
   * copied. So subsequent changes in <tt>values</tt> are not reflected in the
   * matrix, and vice-versa.
   *
   * @param values
   *            The values to be filled into the new matrix.
   * @return a new matrix.
   */
  def make(values: cern.colt.list.tdouble.AbstractDoubleList): StrideMatrix1D = {
    val size = values.size
    val vector = make(size)
    var i = size
    while (i >= 0) vector.set(i, values.get(i))
    vector
  }

  /**
   * Constructs a matrix with the given cell values. The values are copied. So
   * subsequent changes in <tt>values</tt> are not reflected in the matrix,
   * and vice-versa.
   *
   * @param values
   *            The values to be filled into the new matrix.
   */
  def make(values: Array[Double]): StrideMatrix1D = {
    if (this == sparse) new SparseDoubleMatrix1D(values) else new DenseMatrix1D(values)
  }

  /**
   * Constructs a matrix which is the concatenation of all given parts. Cells
   * are copied.
   */
  def make(parts: Array[StrideMatrix1D]): StrideMatrix1D = {
    if (parts.length == 0) return make(0)
    var size = 0
    for (i <- 0 until parts.length) size += parts(i).size
    val vector = make(size)
    size = 0
    for (i <- 0 until parts.length) {
      vector.viewPart(size, parts(i).size.toInt).assign(parts(i))
      size += parts(i).size
    }
    vector
  }

  /**
   * Constructs a matrix with the given shape, each cell initialized with
   * zero.
   */
  def make(size: Int): StrideMatrix1D = {
    if (this == sparse) return new SparseDoubleMatrix1D(size)
    new DenseMatrix1D(size)
  }

  /**
   * Constructs a matrix with the given shape, each cell initialized with the
   * given value.
   */
  def make(size: Int, initialValue: Double): StrideMatrix1D = make(size).assign(initialValue)

  /**
   * Constructs a matrix with uniformly distributed values in <tt>(0,1)</tt>
   * (exclusive).
   */
  def random(size: Int): StrideMatrix1D = {
    make(size).assign(cern.jet.math.tdouble.DoubleFunctions.random())
  }

  /**
   * C = A||A||..||A; Constructs a new matrix which is concatenated
   * <tt>repeat</tt> times. Example:
   *
   * <pre>
   * 	 0 1
   * 	 repeat(3) --&gt;
   * 	 0 1 0 1 0 1
   *
   * </pre>
   */
  def repeat(A: StrideMatrix1D, repeat: Int): StrideMatrix1D = {
    val size = A.size.toInt
    val matrix = make(repeat * size)
    var i = repeat
    while (i >= 0) {
      matrix.viewPart(size * i, size).assign(A)
    }
    matrix
  }

  /**
   * Constructs a randomly sampled matrix with the given shape. Randomly picks
   * exactly <tt>Math.round(size*nonZeroFraction)</tt> cells and initializes
   * them to <tt>value</tt>, all the rest will be initialized to zero. Note
   * that this is not the same as setting each cell with probability
   * <tt>nonZeroFraction</tt> to <tt>value</tt>.
   *
   * @throws IllegalArgumentException
   *             if <tt>nonZeroFraction < 0 || nonZeroFraction > 1</tt>.
   * @see cern.jet.random.tdouble.sampling.DoubleRandomSampler
   */
  def sample(size: Int, value: Double, nonZeroFraction: Double): StrideMatrix1D = {
    val epsilon = 1e-09
    if (nonZeroFraction < 0 - epsilon || nonZeroFraction > 1 + epsilon) throw new IllegalArgumentException()
    if (nonZeroFraction < 0) nonZeroFraction = 0
    if (nonZeroFraction > 1) nonZeroFraction = 1
    val matrix = make(size)
    val n = Math.round(size * nonZeroFraction).toInt
    if (n == 0) return matrix
    val sampler = new cern.jet.random.tdouble.sampling.DoubleRandomSamplingAssistant(n, size, new cern.jet.random.tdouble.engine.DoubleMersenneTwister())
    var i = size
    while (i >= 0) {
      if (sampler.sampleNextElement()) {
        matrix.setQuick(i, value)
      }
    }
    matrix
  }

  /**
   * Constructs a list from the given matrix. The values are copied. So
   * subsequent changes in <tt>values</tt> are not reflected in the list, and
   * vice-versa.
   *
   * @param values
   *            The values to be filled into the new list.
   * @return a new list.
   */
  def toList(values: StrideMatrix1D): cern.colt.list.tdouble.DoubleArrayList = {
    val size = values.size.toInt
    val list = new cern.colt.list.tdouble.DoubleArrayList(size)
    list.setSize(size)
    var i = size
    while (i >= 0) list.setQuick(i, values.get(i))
    list
  }
}
