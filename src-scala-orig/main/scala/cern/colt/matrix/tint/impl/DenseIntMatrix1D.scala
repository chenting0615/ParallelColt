package cern.colt.matrix.tint.impl

import java.util.concurrent.Callable
import java.util.concurrent.ExecutionException
import java.util.concurrent.Future
import cern.colt.list.tint.IntArrayList
import cern.colt.matrix.tint.IntMatrix1D
import cern.colt.matrix.tint.IntMatrix2D
import cern.colt.matrix.tint.IntMatrix3D
import edu.emory.mathcs.utils.ConcurrencyUtils
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Dense 1-d matrix (aka <i>vector</i>) holding <tt>int</tt> elements. First see
 * the <a href="package-summary.html">package summary</a> and javadoc <a
 * href="package-tree.html">tree view</a> to get the broad picture.
 * <p>
 * <b>Implementation:</b>
 * <p>
 * Internally holds one single contigous one-dimensional array. Note that this
 * implementation is not synchronized.
 * <p>
 * <b>Memory requirements:</b>
 * <p>
 * <tt>memory [bytes] = 8*size()</tt>. Thus, a 1000000 matrix uses 8 MB.
 * <p>
 * <b>Time complexity:</b>
 * <p>
 * <tt>O(1)</tt> (i.e. constant time) for the basic operations <tt>get</tt>,
 * <tt>getQuick</tt>, <tt>set</tt>, <tt>setQuick</tt> and <tt>size</tt>,
 * <p>
 *
 * @author wolfgang.hoschek@cern.ch
 * @version 1.0, 09/24/99
 *
 * @author Piotr Wendykier (piotr.wendykier@gmail.com)
 *
 */
@SerialVersionUID(1L)
class DenseIntMatrix1D(size: Int) extends IntMatrix1D {

  /**
   * The elements of this matrix.
   */
  protected var elements: Array[Int] = new Array[Int](size)

  setUp(size)

  /**
   * Constructs a matrix with a copy of the given values. The values are
   * copied. So subsequent changes in <tt>values</tt> are not reflected in the
   * matrix, and vice-versa.
   *
   * @param values
   *            The values to be filled into the new matrix.
   */
  def this(values: Array[Int]) {
    this(values.length)
    assign(values)
  }

  /**
   * Constructs a matrix with the given parameters.
   *
   * @param size
   *            the number of cells the matrix shall have.
   * @param elements
   *            the cells.
   * @param zero
   *            the index of the first element.
   * @param stride
   *            the number of indexes between any two elements, i.e.
   *            <tt>index(i+1)-index(i)</tt>.
   * @param isView
   *            if true then a matrix view is constructed
   * @throws IllegalArgumentException
   *             if <tt>size<0</tt>.
   */
  def this(size: Int, 
      elements: Array[Int], 
      zero: Int, 
      stride: Int, 
      isView: Boolean) {
    this()
    setUp(size, zero, stride)
    this.elements = elements
    this.isNoView = !isView
  }

  def aggregate(aggr: cern.colt.function.tint.IntIntFunction, f: cern.colt.function.tint.IntFunction): Int = {
    if (size == 0) throw new IllegalArgumentException("size == 0")
    var a = 0
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_1D)) {
      nthreads = Math.min(nthreads, size)
      val futures = Array.ofDim[Future](nthreads)
      val k = size / nthreads
      for (j <- 0 until nthreads) {
        val firstIdx = j * k
        val lastIdx = if ((j == nthreads - 1)) size else firstIdx + k
        futures(j) = ConcurrencyUtils.submit(new Callable[Integer]() {

          def call(): java.lang.Integer = {
            var idx = zero + firstIdx * stride
            var a = f.apply(elements(idx))
            for (i <- firstIdx + 1 until lastIdx) {
              idx += stride
              a = aggr.apply(a, f.apply(elements(idx)))
            }
            return a
          }
        })
      }
      a = ConcurrencyUtils.waitForCompletion(futures, aggr)
    } else {
      a = f.apply(elements(zero))
      var idx = zero
      for (i <- 1 until size) {
        idx += stride
        a = aggr.apply(a, f.apply(elements(idx)))
      }
    }
    a
  }

  def aggregate(aggr: cern.colt.function.tint.IntIntFunction, f: cern.colt.function.tint.IntFunction, indexList: IntArrayList): Int = {
    if (size == 0) throw new IllegalArgumentException("size == 0")
    val size = indexList.size
    val indexElements = indexList.elements()
    var a = 0
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_1D)) {
      nthreads = Math.min(nthreads, size)
      val futures = Array.ofDim[Future](nthreads)
      val k = size / nthreads
      for (j <- 0 until nthreads) {
        val firstIdx = j * k
        val lastIdx = if ((j == nthreads - 1)) size else firstIdx + k
        futures(j) = ConcurrencyUtils.submit(new Callable[Integer]() {

          def call(): java.lang.Integer = {
            var idx = zero + indexElements(firstIdx) * stride
            var a = f.apply(elements(idx))
            var elem: Int = 0
            for (i <- firstIdx + 1 until lastIdx) {
              idx = zero + indexElements(i) * stride
              elem = elements(idx)
              a = aggr.apply(a, f.apply(elem))
            }
            return a
          }
        })
      }
      a = ConcurrencyUtils.waitForCompletion(futures, aggr)
    } else {
      var elem: Int = 0
      var idx = zero + indexElements(0) * stride
      a = f.apply(elements(idx))
      for (i <- 1 until size) {
        idx = zero + indexElements(i) * stride
        elem = elements(idx)
        a = aggr.apply(a, f.apply(elem))
      }
    }
    a
  }

  def aggregate(other: IntMatrix1D, aggr: cern.colt.function.tint.IntIntFunction, f: cern.colt.function.tint.IntIntFunction): Int = {
    if (!(other.isInstanceOf[DenseIntMatrix1D])) {
      return super.aggregate(other, aggr, f)
    }
    checkSize(other)
    if (size == 0) throw new IllegalArgumentException("size == 0")
    val zeroOther = other.index(0).toInt
    val strideOther = other.stride()
    val elemsOther = other.elements().asInstanceOf[Array[Int]]
    var a = 0
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_1D)) {
      nthreads = Math.min(nthreads, size)
      val futures = Array.ofDim[Future](nthreads)
      val k = size / nthreads
      for (j <- 0 until nthreads) {
        val firstIdx = j * k
        val lastIdx = if ((j == nthreads - 1)) size else firstIdx + k
        futures(j) = ConcurrencyUtils.submit(new Callable[Integer]() {

          def call(): java.lang.Integer = {
            var idx = zero + firstIdx * stride
            var idxOther = zeroOther + firstIdx * strideOther
            var a = f.apply(elements(idx), elemsOther(idxOther))
            for (i <- firstIdx + 1 until lastIdx) {
              idx += stride
              idxOther += strideOther
              a = aggr.apply(a, f.apply(elements(idx), elemsOther(idxOther)))
            }
            return a
          }
        })
      }
      a = ConcurrencyUtils.waitForCompletion(futures, aggr)
    } else {
      a = f.apply(elements(zero), elemsOther(zeroOther))
      var idx = zero
      var idxOther = zeroOther
      for (i <- 1 until size) {
        idx += stride
        idxOther += strideOther
        a = aggr.apply(a, f.apply(elements(idx), elemsOther(idxOther)))
      }
    }
    a
  }

  def assign(function: cern.colt.function.tint.IntFunction): IntMatrix1D = {
    var multiplicator: Int = 0
    if (function.isInstanceOf[cern.jet.math.tint.IntMult]) {
      multiplicator = function.asInstanceOf[cern.jet.math.tint.IntMult].multiplicator
      if (multiplicator == 1) {
        return this
      }
    } else {
      multiplicator = 0
    }
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_1D)) {
      nthreads = Math.min(nthreads, size)
      val futures = Array.ofDim[Future](nthreads)
      val k = size / nthreads
      for (j <- 0 until nthreads) {
        val firstIdx = j * k
        val lastIdx = if ((j == nthreads - 1)) size else firstIdx + k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            var idx = zero + firstIdx * stride
            if (function.isInstanceOf[cern.jet.math.tint.IntMult]) {
              for (k <- firstIdx until lastIdx) {
                elements(idx) *= multiplicator
                idx += stride
              }
            } else {
              for (k <- firstIdx until lastIdx) {
                elements(idx) = function.apply(elements(idx))
                idx += stride
              }
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
    } else {
      val idx = zero - stride
      if (function.isInstanceOf[cern.jet.math.tint.IntMult]) {
        var k = size
        while (k >= 0) {
          elements(idx += stride) *= multiplicator
        }
      } else {
        var k = size
        while (k >= 0) {
          elements(idx += stride) = function.apply(elements(idx))
        }
      }
    }
    this
  }

  def assign(cond: cern.colt.function.tint.IntProcedure, function: cern.colt.function.tint.IntFunction): IntMatrix1D = {
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_1D)) {
      nthreads = Math.min(nthreads, size)
      val futures = Array.ofDim[Future](nthreads)
      val k = size / nthreads
      for (j <- 0 until nthreads) {
        val firstIdx = j * k
        val lastIdx = if ((j == nthreads - 1)) size else firstIdx + k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            var idx = zero + firstIdx * stride
            for (i <- firstIdx until lastIdx) {
              if (cond.apply(elements(idx)) == true) {
                elements(idx) = function.apply(elements(idx))
              }
              idx += stride
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
    } else {
      var idx = zero
      for (i <- 0 until size) {
        if (cond.apply(elements(idx)) == true) {
          elements(idx) = function.apply(elements(idx))
        }
        idx += stride
      }
    }
    this
  }

  def assign(cond: cern.colt.function.tint.IntProcedure, value: Int): IntMatrix1D = {
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_1D)) {
      nthreads = Math.min(nthreads, size)
      val futures = Array.ofDim[Future](nthreads)
      val k = size / nthreads
      for (j <- 0 until nthreads) {
        val firstIdx = j * k
        val lastIdx = if ((j == nthreads - 1)) size else firstIdx + k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            var idx = zero + firstIdx * stride
            for (i <- firstIdx until lastIdx) {
              if (cond.apply(elements(idx)) == true) {
                elements(idx) = value
              }
              idx += stride
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
    } else {
      var idx = zero
      for (i <- 0 until size) {
        if (cond.apply(elements(idx)) == true) {
          elements(idx) = value
        }
        idx += stride
      }
    }
    this
  }

  def assign(value: Int): IntMatrix1D = {
    val elems = this.elements
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_1D)) {
      nthreads = Math.min(nthreads, size)
      val futures = Array.ofDim[Future](nthreads)
      val k = size / nthreads
      for (j <- 0 until nthreads) {
        val firstIdx = j * k
        val lastIdx = if ((j == nthreads - 1)) size else firstIdx + k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            var idx = zero + firstIdx * stride
            for (k <- firstIdx until lastIdx) {
              elems(idx) = value
              idx += stride
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
    } else {
      var idx = zero
      for (i <- 0 until size) {
        elems(idx) = value
        idx += stride
      }
    }
    this
  }

  def assign(values: Array[Int]): IntMatrix1D = {
    if (values.length != size) throw new IllegalArgumentException("Must have same number of cells: length=" + values.length + 
      "size()=" + 
      size)
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if (isNoView) {
      System.arraycopy(values, 0, this.elements, 0, values.length)
    } else {
      if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_1D)) {
        nthreads = Math.min(nthreads, size)
        val futures = Array.ofDim[Future](nthreads)
        val k = size / nthreads
        for (j <- 0 until nthreads) {
          val firstIdx = j * k
          var lastIdx: Int = 0
          lastIdx = if (j == nthreads - 1) size else firstIdx + k
          futures(j) = ConcurrencyUtils.submit(new Runnable() {

            def run() {
              var idx = zero + firstIdx * stride
              for (i <- firstIdx until lastIdx) {
                elements(idx) = values(i)
                idx += stride
              }
            }
          })
        }
        ConcurrencyUtils.waitForCompletion(futures)
      } else {
        var idx = zero
        for (i <- 0 until size) {
          elements(idx) = values(i)
          idx += stride
        }
      }
    }
    this
  }

  def assign(source: IntMatrix1D): IntMatrix1D = {
    if (!(source.isInstanceOf[DenseIntMatrix1D])) {
      super.assign(source)
      return this
    }
    var other = source.asInstanceOf[DenseIntMatrix1D]
    if (other == this) return this
    checkSize(other)
    if (isNoView && other.isNoView) {
      System.arraycopy(other.elements, 0, this.elements, 0, this.elements.length)
      return this
    }
    if (haveSharedCells(other)) {
      val c = other.copy()
      if (!(c.isInstanceOf[DenseIntMatrix1D])) {
        super.assign(source)
        return this
      }
      other = c.asInstanceOf[DenseIntMatrix1D]
    }
    val elemsOther = other.elements
    if (elements == null || elemsOther == null) throw new InternalError()
    val zeroOther = other.index(0).toInt
    val strideOther = other.stride
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_1D)) {
      nthreads = Math.min(nthreads, size)
      val futures = Array.ofDim[Future](nthreads)
      val k = size / nthreads
      for (j <- 0 until nthreads) {
        val firstIdx = j * k
        val lastIdx = if ((j == nthreads - 1)) size else firstIdx + k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            var idx = zero + firstIdx * stride
            var idxOther = zeroOther + firstIdx * strideOther
            for (k <- firstIdx until lastIdx) {
              elements(idx) = elemsOther(idxOther)
              idx += stride
              idxOther += strideOther
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
    } else {
      var idx = zero
      var idxOther = zeroOther
      for (k <- 0 until size) {
        elements(idx) = elemsOther(idxOther)
        idx += stride
        idxOther += strideOther
      }
    }
    this
  }

  def assign(y: IntMatrix1D, function: cern.colt.function.tint.IntIntFunction): IntMatrix1D = {
    if (!(y.isInstanceOf[DenseIntMatrix1D])) {
      super.assign(y, function)
      return this
    }
    checkSize(y)
    val zeroOther = y.index(0).toInt
    val strideOther = y.stride()
    val elemsOther = y.elements().asInstanceOf[Array[Int]]
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_1D)) {
      nthreads = Math.min(nthreads, size)
      val futures = Array.ofDim[Future](nthreads)
      val k = size / nthreads
      for (j <- 0 until nthreads) {
        val firstIdx = j * k
        val lastIdx = if ((j == nthreads - 1)) size else firstIdx + k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            var idx = zero + firstIdx * stride
            var idxOther = zeroOther + firstIdx * strideOther
            if (function == cern.jet.math.tint.IntFunctions.mult) {
              for (k <- firstIdx until lastIdx) {
                elements(idx) *= elemsOther(idxOther)
                idx += stride
                idxOther += strideOther
              }
            } else if (function == cern.jet.math.tint.IntFunctions.div) {
              for (k <- firstIdx until lastIdx) {
                elements(idx) /= elemsOther(idxOther)
                idx += stride
                idxOther += strideOther
              }
            } else if (function.isInstanceOf[cern.jet.math.tint.IntPlusMultFirst]) {
              var multiplicator = function.asInstanceOf[cern.jet.math.tint.IntPlusMultFirst].multiplicator
              if (multiplicator == 0) {
                for (k <- firstIdx until lastIdx) {
                  elements(idx) = elemsOther(idxOther)
                  idx += stride
                  idxOther += strideOther
                }
              } else if (multiplicator == 1) {
                for (k <- firstIdx until lastIdx) {
                  elements(idx) += elemsOther(idxOther)
                  idx += stride
                  idxOther += strideOther
                }
              } else if (multiplicator == -1) {
                for (k <- firstIdx until lastIdx) {
                  elements(idx) = elemsOther(idxOther) - elements(idx)
                  idx += stride
                  idxOther += strideOther
                }
              } else {
                for (k <- firstIdx until lastIdx) {
                  elements(idx) = multiplicator * elements(idx) + elemsOther(idxOther)
                  idx += stride
                  idxOther += strideOther
                }
              }
            } else if (function.isInstanceOf[cern.jet.math.tint.IntPlusMultSecond]) {
              var multiplicator = function.asInstanceOf[cern.jet.math.tint.IntPlusMultSecond].multiplicator
              if (multiplicator == 0) {
                return
              } else if (multiplicator == 1) {
                for (k <- firstIdx until lastIdx) {
                  elements(idx) += elemsOther(idxOther)
                  idx += stride
                  idxOther += strideOther
                }
              } else if (multiplicator == -1) {
                for (k <- firstIdx until lastIdx) {
                  elements(idx) -= elemsOther(idxOther)
                  idx += stride
                  idxOther += strideOther
                }
              } else {
                for (k <- firstIdx until lastIdx) {
                  elements(idx) += multiplicator * elemsOther(idxOther)
                  idx += stride
                  idxOther += strideOther
                }
              }
            } else {
              for (k <- firstIdx until lastIdx) {
                elements(idx) = function.apply(elements(idx), elemsOther(idxOther))
                idx += stride
                idxOther += strideOther
              }
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
    } else {
      var idx = zero
      var idxOther = zeroOther
      if (function == cern.jet.math.tint.IntFunctions.mult) {
        for (k <- 0 until size) {
          elements(idx) *= elemsOther(idxOther)
          idx += stride
          idxOther += strideOther
        }
      } else if (function == cern.jet.math.tint.IntFunctions.div) {
        for (k <- 0 until size) {
          elements(idx) /= elemsOther(idxOther)
          idx += stride
          idxOther += strideOther
        }
      } else if (function.isInstanceOf[cern.jet.math.tint.IntPlusMultSecond]) {
        val multiplicator = function.asInstanceOf[cern.jet.math.tint.IntPlusMultSecond].multiplicator
        if (multiplicator == 0) {
          return this
        } else if (multiplicator == 1) {
          for (k <- 0 until size) {
            elements(idx) += elemsOther(idxOther)
            idx += stride
            idxOther += strideOther
          }
        } else if (multiplicator == -1) {
          for (k <- 0 until size) {
            elements(idx) -= elemsOther(idxOther)
            idx += stride
            idxOther += strideOther
          }
        } else {
          for (k <- 0 until size) {
            elements(idx) += multiplicator * elemsOther(idxOther)
            idx += stride
            idxOther += strideOther
          }
        }
      } else {
        for (k <- 0 until size) {
          elements(idx) = function.apply(elements(idx), elemsOther(idxOther))
          idx += stride
          idxOther += strideOther
        }
      }
    }
    this
  }

  def cardinality(): Int = {
    var cardinality = 0
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_1D)) {
      nthreads = Math.min(nthreads, size)
      val futures = Array.ofDim[Future](nthreads)
      val results = Array.ofDim[Integer](nthreads)
      val k = size / nthreads
      for (j <- 0 until nthreads) {
        val firstIdx = j * k
        val lastIdx = if ((j == nthreads - 1)) size else firstIdx + k
        futures(j) = ConcurrencyUtils.submit(new Callable[Integer]() {

          def call(): java.lang.Integer = {
            var cardinality = 0
            var idx = zero + firstIdx * stride
            for (i <- firstIdx until lastIdx) {
              if (elements(idx) != 0) cardinality += 1
              idx += stride
            }
            return cardinality
          }
        })
      }
      try {
        for (j <- 0 until nthreads) {
          results(j) = futures(j).get.asInstanceOf[java.lang.Integer]
        }
        cardinality = results(0)
        for (j <- 1 until nthreads) {
          cardinality += results(j)
        }
      } catch {
        case ex: ExecutionException => ex.printStackTrace()
        case e: InterruptedException => e.printStackTrace()
      }
    } else {
      var idx = zero
      for (i <- 0 until size) {
        if (elements(idx) != 0) cardinality += 1
        idx += stride
      }
    }
    cardinality
  }

  def elements(): Array[Int] = elements

  def getNonZeros(indexList: IntArrayList, valueList: IntArrayList) {
    indexList.clear()
    valueList.clear()
    var idx = zero
    val rem = size % 2
    if (rem == 1) {
      val value = elements(idx)
      if (value != 0) {
        indexList.add(0)
        valueList.add(value)
      }
      idx += stride
    }
    var i = rem
    while (i < size) {
      var value = elements(idx)
      if (value != 0) {
        indexList.add(i)
        valueList.add(value)
      }
      idx += stride
      value = elements(idx)
      if (value != 0) {
        indexList.add(i + 1)
        valueList.add(value)
      }
      idx += stride
      i += 2
    }
  }

  def getPositiveValues(indexList: IntArrayList, valueList: IntArrayList) {
    indexList.clear()
    valueList.clear()
    var idx = zero
    val rem = size % 2
    if (rem == 1) {
      val value = elements(idx)
      if (value > 0) {
        indexList.add(0)
        valueList.add(value)
      }
      idx += stride
    }
    var i = rem
    while (i < size) {
      var value = elements(idx)
      if (value > 0) {
        indexList.add(i)
        valueList.add(value)
      }
      idx += stride
      value = elements(idx)
      if (value > 0) {
        indexList.add(i + 1)
        valueList.add(value)
      }
      idx += stride
      i += 2
    }
  }

  def getNegativeValues(indexList: IntArrayList, valueList: IntArrayList) {
    indexList.clear()
    valueList.clear()
    var idx = zero
    val rem = size % 2
    if (rem == 1) {
      val value = elements(idx)
      if (value < 0) {
        indexList.add(0)
        valueList.add(value)
      }
      idx += stride
    }
    var i = rem
    while (i < size) {
      var value = elements(idx)
      if (value < 0) {
        indexList.add(i)
        valueList.add(value)
      }
      idx += stride
      value = elements(idx)
      if (value < 0) {
        indexList.add(i + 1)
        valueList.add(value)
      }
      idx += stride
      i += 2
    }
  }

  def getMaxLocation(): Array[Int] = {
    var location = 0
    var maxValue = 0
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_1D)) {
      nthreads = Math.min(nthreads, size)
      val futures = Array.ofDim[Future](nthreads)
      val results = Array.ofDim[Int](nthreads, 2)
      val k = size / nthreads
      for (j <- 0 until nthreads) {
        val firstIdx = j * k
        val lastIdx = if ((j == nthreads - 1)) size else firstIdx + k
        futures(j) = ConcurrencyUtils.submit(new Callable[Array[Int]]() {

          def call(): Array[Int] = {
            var idx = zero + firstIdx * stride
            var maxValue = elements(idx)
            var location = (idx - zero) / stride
            for (i <- firstIdx + 1 until lastIdx) {
              idx += stride
              if (maxValue < elements(idx)) {
                maxValue = elements(idx)
                location = (idx - zero) / stride
              }
            }
            return Array(maxValue, location)
          }
        })
      }
      try {
        for (j <- 0 until nthreads) {
          results(j) = futures(j).get.asInstanceOf[Array[Int]]
        }
        maxValue = results(0)(0)
        location = results(0)(1).toInt
        for (j <- 1 until nthreads if maxValue < results(j)(0)) {
          maxValue = results(j)(0)
          location = results(j)(1).toInt
        }
      } catch {
        case ex: ExecutionException => ex.printStackTrace()
        case e: InterruptedException => e.printStackTrace()
      }
    } else {
      maxValue = elements(zero)
      location = 0
      var idx = zero
      for (i <- 1 until size) {
        idx += stride
        if (maxValue < elements(idx)) {
          maxValue = elements(idx)
          location = (idx - zero) / stride
        }
      }
    }
    Array(maxValue, location)
  }

  def getMinLocation(): Array[Int] = {
    var location = 0
    var minValue = 0
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_1D)) {
      nthreads = Math.min(nthreads, size)
      val futures = Array.ofDim[Future](nthreads)
      val results = Array.ofDim[Int](nthreads, 2)
      val k = size / nthreads
      for (j <- 0 until nthreads) {
        val firstIdx = j * k
        val lastIdx = if ((j == nthreads - 1)) size else firstIdx + k
        futures(j) = ConcurrencyUtils.submit(new Callable[Array[Int]]() {

          def call(): Array[Int] = {
            var idx = zero + firstIdx * stride
            var minValue = elements(idx)
            var location = (idx - zero) / stride
            for (i <- firstIdx + 1 until lastIdx) {
              idx += stride
              if (minValue > elements(idx)) {
                minValue = elements(idx)
                location = (idx - zero) / stride
              }
            }
            return Array(minValue, location)
          }
        })
      }
      try {
        for (j <- 0 until nthreads) {
          results(j) = futures(j).get.asInstanceOf[Array[Int]]
        }
        minValue = results(0)(0)
        location = results(0)(1).toInt
        for (j <- 1 until nthreads if minValue > results(j)(0)) {
          minValue = results(j)(0)
          location = results(j)(1).toInt
        }
      } catch {
        case ex: ExecutionException => ex.printStackTrace()
        case e: InterruptedException => e.printStackTrace()
      }
    } else {
      minValue = elements(zero)
      location = 0
      var idx = zero
      for (i <- 1 until size) {
        idx += stride
        if (minValue > elements(idx)) {
          minValue = elements(idx)
          location = (idx - zero) / stride
        }
      }
    }
    Array(minValue, location)
  }

  def getQuick(index: Int): Int = elements(zero + index * stride)

  def like(size: Int): IntMatrix1D = new DenseIntMatrix1D(size)

  def like2D(rows: Int, columns: Int): IntMatrix2D = new DenseIntMatrix2D(rows, columns)

  def reshape(rows: Int, columns: Int): IntMatrix2D = {
    if (rows * columns != size) {
      throw new IllegalArgumentException("rows*columns != size")
    }
    val M = new DenseIntMatrix2D(rows, columns)
    val elemsOther = M.elements().asInstanceOf[Array[Int]]
    val zeroOther = M.index(0, 0).toInt
    val rowStrideOther = M.rowStride()
    val colStrideOther = M.columnStride()
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_1D)) {
      nthreads = Math.min(nthreads, columns)
      val futures = Array.ofDim[Future](nthreads)
      val k = columns / nthreads
      for (j <- 0 until nthreads) {
        val firstColumn = j * k
        val lastColumn = if ((j == nthreads - 1)) columns else firstColumn + k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            var idx: Int = 0
            var idxOther: Int = 0
            for (c <- firstColumn until lastColumn) {
              idxOther = zeroOther + c * colStrideOther
              idx = zero + (c * rows) * stride
              for (r <- 0 until rows) {
                elemsOther(idxOther) = elements(idx)
                idxOther += rowStrideOther
                idx += stride
              }
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
    } else {
      var idxOther: Int = 0
      var idx = zero
      for (c <- 0 until columns) {
        idxOther = zeroOther + c * colStrideOther
        for (r <- 0 until rows) {
          elemsOther(idxOther) = elements(idx)
          idxOther += rowStrideOther
          idx += stride
        }
      }
    }
    M
  }

  def reshape(slices: Int, rows: Int, columns: Int): IntMatrix3D = {
    if (slices * rows * columns != size) {
      throw new IllegalArgumentException("slices*rows*columns != size")
    }
    val M = new DenseIntMatrix3D(slices, rows, columns)
    val elemsOther = M.elements().asInstanceOf[Array[Int]]
    val zeroOther = M.index(0, 0, 0).toInt
    val sliceStrideOther = M.sliceStride()
    val rowStrideOther = M.rowStride()
    val colStrideOther = M.columnStride()
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_1D)) {
      nthreads = Math.min(nthreads, slices)
      val futures = Array.ofDim[Future](nthreads)
      val k = slices / nthreads
      for (j <- 0 until nthreads) {
        val firstSlice = j * k
        val lastSlice = if ((j == nthreads - 1)) slices else firstSlice + k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            var idx: Int = 0
            var idxOther: Int = 0
            for (s <- firstSlice until lastSlice; c <- 0 until columns) {
              idxOther = zeroOther + s * sliceStrideOther + c * colStrideOther
              idx = zero + (s * rows * columns + c * rows) * stride
              for (r <- 0 until rows) {
                elemsOther(idxOther) = elements(idx)
                idxOther += rowStrideOther
                idx += stride
              }
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
    } else {
      var idxOther: Int = 0
      var idx = zero
      for (s <- 0 until slices; c <- 0 until columns) {
        idxOther = zeroOther + s * sliceStrideOther + c * colStrideOther
        for (r <- 0 until rows) {
          elemsOther(idxOther) = elements(idx)
          idxOther += rowStrideOther
          idx += stride
        }
      }
    }
    M
  }

  def setQuick(index: Int, value: Int) {
    elements(zero + index * stride) = value
  }

  def swap(other: IntMatrix1D) {
    if (!(other.isInstanceOf[DenseIntMatrix1D])) {
      super.swap(other)
    }
    val y = other.asInstanceOf[DenseIntMatrix1D]
    if (y == this) return
    checkSize(y)
    val elemsOther = y.elements
    if (elements == null || elemsOther == null) throw new InternalError()
    val zeroOther = other.index(0).toInt
    val strideOther = other.stride()
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_1D)) {
      nthreads = Math.min(nthreads, size)
      val futures = Array.ofDim[Future](nthreads)
      val k = size / nthreads
      for (j <- 0 until nthreads) {
        val firstIdx = j * k
        val lastIdx = if ((j == nthreads - 1)) size else firstIdx + k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            var idx = zero + firstIdx * stride
            var idxOther = zeroOther + firstIdx * strideOther
            for (k <- firstIdx until lastIdx) {
              var tmp = elements(idx)
              elements(idx) = elemsOther(idxOther)
              elemsOther(idxOther) = tmp
              idx += stride
              idxOther += strideOther
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
    } else {
      var idx = zero
      var idxOther = zeroOther
      for (k <- 0 until size) {
        val tmp = elements(idx)
        elements(idx) = elemsOther(idxOther)
        elemsOther(idxOther) = tmp
        idx += stride
        idxOther += strideOther
      }
    }
  }

  def toArray(values: Array[Int]) {
    if (values.length < size) throw new IllegalArgumentException("values too small")
    if (this.isNoView) System.arraycopy(this.elements, 0, values, 0, this.elements.length) else super.toArray(values)
  }

  def zDotProduct(y: IntMatrix1D): Int = {
    if (!(y.isInstanceOf[DenseIntMatrix1D])) {
      return super.zDotProduct(y)
    }
    val yy = y.asInstanceOf[DenseIntMatrix1D]
    val elemsOther = yy.elements
    var zeroThis = index(0).toInt
    var zeroOther = yy.index(0).toInt
    val strideOther = yy.stride
    if (elements == null || elemsOther == null) throw new InternalError()
    var sum = 0
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_1D)) {
      val zeroThisF = zeroThis
      val zeroOtherF = zeroOther
      val strideOtherF = strideOther
      nthreads = Math.min(nthreads, size)
      val futures = Array.ofDim[Future](nthreads)
      val results = Array.ofDim[Integer](nthreads)
      val k = size / nthreads
      for (j <- 0 until nthreads) {
        val firstIdx = j * k
        val lastIdx = if ((j == nthreads - 1)) size else firstIdx + k
        futures(j) = ConcurrencyUtils.submit(new Callable[Integer]() {

          def call(): java.lang.Integer = {
            var idx = zeroThisF + firstIdx * stride
            var idxOther = zeroOtherF + firstIdx * strideOtherF
            idx -= stride
            idxOther -= strideOtherF
            var sum = 0
            var min = lastIdx - firstIdx
            var k = min / 4
            while (k >= 0) {
              sum += elements(idx += stride) * elemsOther(idxOther += strideOtherF) + 
                elements(idx += stride) * elemsOther(idxOther += strideOtherF) + 
                elements(idx += stride) * elemsOther(idxOther += strideOtherF) + 
                elements(idx += stride) * elemsOther(idxOther += strideOtherF)
            }
            var k = min % 4
            while (k >= 0) {
              sum += elements(idx += stride) * elemsOther(idxOther += strideOtherF)
            }
            return sum
          }
        })
      }
      try {
        for (j <- 0 until nthreads) {
          results(j) = futures(j).get.asInstanceOf[java.lang.Integer]
        }
        sum = results(0)
        for (j <- 1 until nthreads) {
          sum += results(j)
        }
      } catch {
        case ex: ExecutionException => ex.printStackTrace()
        case e: InterruptedException => e.printStackTrace()
      }
    } else {
      zeroThis -= stride
      zeroOther -= strideOther
      var k = size / 4
      while (k >= 0) {
        sum += elements(zeroThis += stride) * elemsOther(zeroOther += strideOther) + 
          elements(zeroThis += stride) * elemsOther(zeroOther += strideOther) + 
          elements(zeroThis += stride) * elemsOther(zeroOther += strideOther) + 
          elements(zeroThis += stride) * elemsOther(zeroOther += strideOther)
      }
      var k = size % 4
      while (k >= 0) {
        sum += elements(zeroThis += stride) * elemsOther(zeroOther += strideOther)
      }
    }
    sum
  }

  def zDotProduct(y: IntMatrix1D, from: Int, length: Int): Int = {
    if (!(y.isInstanceOf[DenseIntMatrix1D])) {
      return super.zDotProduct(y, from, length)
    }
    val yy = y.asInstanceOf[DenseIntMatrix1D]
    var tail = from + length
    if (from < 0 || length < 0) return 0
    if (size < tail) tail = size
    if (y.size < tail) tail = y.size.toInt
    val elementsOther = yy.elements
    var zeroThis = index(from).toInt
    var zeroOther = yy.index(from).toInt
    val strideOther = yy.stride
    if (elements == null || elementsOther == null) throw new InternalError()
    var sum = 0
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (length >= ConcurrencyUtils.getThreadsBeginN_1D)) {
      val zeroThisF = zeroThis
      val zeroOtherF = zeroOther
      val strideOtherF = strideOther
      nthreads = Math.min(nthreads, length)
      val futures = Array.ofDim[Future](nthreads)
      val results = Array.ofDim[Integer](nthreads)
      val k = length / nthreads
      for (j <- 0 until nthreads) {
        val firstIdx = j * k
        val lastIdx = if ((j == nthreads - 1)) length else firstIdx + k
        futures(j) = ConcurrencyUtils.submit(new Callable[Integer]() {

          def call(): java.lang.Integer = {
            var idx = zeroThisF + firstIdx * stride
            var idxOther = zeroOtherF + firstIdx * strideOtherF
            idx -= stride
            idxOther -= strideOtherF
            var sum = 0
            var min = lastIdx - firstIdx
            var k = min / 4
            while (k >= 0) {
              sum += elements(idx += stride) * elementsOther(idxOther += strideOtherF) + 
                elements(idx += stride) * elementsOther(idxOther += strideOtherF) + 
                elements(idx += stride) * elementsOther(idxOther += strideOtherF) + 
                elements(idx += stride) * elementsOther(idxOther += strideOtherF)
            }
            var k = min % 4
            while (k >= 0) {
              sum += elements(idx += stride) * elementsOther(idxOther += strideOtherF)
            }
            return sum
          }
        })
      }
      try {
        for (j <- 0 until nthreads) {
          results(j) = futures(j).get.asInstanceOf[java.lang.Integer]
        }
        sum = results(0)
        for (j <- 1 until nthreads) {
          sum += results(j)
        }
      } catch {
        case ex: ExecutionException => ex.printStackTrace()
        case e: InterruptedException => e.printStackTrace()
      }
    } else {
      zeroThis -= stride
      zeroOther -= strideOther
      val min = tail - from
      var k = min / 4
      while (k >= 0) {
        sum += elements(zeroThis += stride) * elementsOther(zeroOther += strideOther) + 
          elements(zeroThis += stride) * elementsOther(zeroOther += strideOther) + 
          elements(zeroThis += stride) * elementsOther(zeroOther += strideOther) + 
          elements(zeroThis += stride) * elementsOther(zeroOther += strideOther)
      }
      var k = min % 4
      while (k >= 0) {
        sum += elements(zeroThis += stride) * elementsOther(zeroOther += strideOther)
      }
    }
    sum
  }

  def zSum(): Int = {
    var sum = 0
    val elems = this.elements
    if (elems == null) throw new InternalError()
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_1D)) {
      nthreads = Math.min(nthreads, size)
      val futures = Array.ofDim[Future](nthreads)
      val results = Array.ofDim[Integer](nthreads)
      val k = size / nthreads
      for (j <- 0 until nthreads) {
        val firstIdx = j * k
        val lastIdx = if ((j == nthreads - 1)) size else firstIdx + k
        futures(j) = ConcurrencyUtils.submit(new Callable[Integer]() {

          def call(): java.lang.Integer = {
            var sum = 0
            var idx = zero + firstIdx * stride
            for (i <- firstIdx until lastIdx) {
              sum += elems(idx)
              idx += stride
            }
            return sum
          }
        })
      }
      try {
        for (j <- 0 until nthreads) {
          results(j) = futures(j).get.asInstanceOf[java.lang.Integer]
        }
        sum = results(0)
        for (j <- 1 until nthreads) {
          sum += results(j)
        }
      } catch {
        case ex: ExecutionException => ex.printStackTrace()
        case e: InterruptedException => e.printStackTrace()
      }
    } else {
      var idx = zero
      for (k <- 0 until size) {
        sum += elems(idx)
        idx += stride
      }
    }
    sum
  }

  protected def cardinality(maxCardinality: Int): Int = {
    var cardinality = 0
    var index = zero
    val elems = this.elements
    val i = size
    while (i >= 0 && cardinality < maxCardinality) {
      if (elems(index) != 0) cardinality += 1
      index += stride
    }
    cardinality
  }

  protected def haveSharedCellsRaw(other: IntMatrix1D): Boolean = {
    if (other.isInstanceOf[SelectedDenseIntMatrix1D]) {
      val otherMatrix = other.asInstanceOf[SelectedDenseIntMatrix1D]
      return this.elements == otherMatrix.elements
    } else if (other.isInstanceOf[DenseIntMatrix1D]) {
      val otherMatrix = other.asInstanceOf[DenseIntMatrix1D]
      return this.elements == otherMatrix.elements
    }
    false
  }

  def index(rank: Int): Long = zero + rank * stride

  protected def viewSelectionLike(offsets: Array[Int]): IntMatrix1D = {
    new SelectedDenseIntMatrix1D(this.elements, offsets)
  }
}
