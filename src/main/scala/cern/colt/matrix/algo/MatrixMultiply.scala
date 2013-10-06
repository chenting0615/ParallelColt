package cern.colt.matrix.algo

import cern.colt.matrix.{IndexIterator1D, IndexIterator2D, MatrixProcessor}
import cern.colt.matrix.MatrixTypes.{DoubleMatrix2D, DoubleMatrix1D}
import cern.colt.matrix.MatrixOperators._
import cern.colt.function.ProcedureTypes.IntProcedure
import scala.collection.mutable.ArrayBuffer

/**
  */
object MatrixMultiply {

  trait MultiplyAlgorithm {

    def predicate(A: DoubleMatrix1D, B: DoubleMatrix1D): Boolean = true
    def predicate(A: DoubleMatrix2D, B: DoubleMatrix1D, result: DoubleMatrix1D, transposeA: Boolean): Boolean = true
    def predicate(A: DoubleMatrix2D, B: DoubleMatrix2D, result: DoubleMatrix2D, transposeA: Boolean, transposeB: Boolean): Boolean = true

    /**
     * Linear algebraic matrix-vector multiplication;
     * <tt>result = alpha * A * B</tt>.
     * <tt>result = alpha*Sum(A[i] * B[i]), i=0..A.size()-1</tt>
     * Note: Matrix shape conformance is checked.
     *
     * @param A
     *            the source vector.
     * @param B
     *            the source vector.
     * @return The dot product of the vectors
     *
     * @throws IllegalArgumentException
     *             if <tt>A.size() != y.size()</tt>.
     */
    def dot(A: DoubleMatrix1D, B: DoubleMatrix1D): Double

    /**
     * Linear algebraic matrix-vector multiplication;
     * <tt>result = alpha * A * B</tt>.
     * <tt>result = alpha*Sum(A[i] * B[i]), i=start..end-1</tt>
     * Note: Matrix shape conformance is checked.
     *
     * @param A
     *            the source vector.
     * @param B
     *            the source vector.
     * @return The dot product of the vectors
     *
     * @throws IllegalArgumentException
     *             if <tt>A.size() != y.size()</tt>.
     */
    def dot(A: DoubleMatrix1D, B: DoubleMatrix1D, start: Int, end: Int): Double

    /**
     * Linear algebraic matrix-vector multiplication;
     * <tt>z = alpha * A * y + beta*z</tt>.
     * <tt>z[i] = alpha*Sum(A[i,j] * y[j]) + beta*z[i], i=0..A.rows-1, j=0..y.size()-1</tt>
     * . Where <tt>A == this</tt>. <br>
     * Note: Matrix shape conformance is checked <i>after</i> potential
     * transpositions.
     *
     * @param y
     *            the source vector.
     * @param z
     *            the vector where results are to be stored. Set this parameter
     *            to <tt>null</tt> to indicate that a new result vector shall be
     *            constructed.
     * @return z (for convenience only).
     *
     * @throws IllegalArgumentException
     *             if <tt>A.columns != y.size() || A.rows > z.size())</tt>.
     */
    def multiply(A: DoubleMatrix2D, y: DoubleMatrix1D, z: DoubleMatrix1D, alpha: Double, beta: Double): DoubleMatrix1D

    /**
     * Linear algebraic matrix-matrix multiplication;
     * <tt>C = alpha * A x B + beta*C</tt>.
     * <tt>C[i,j] = alpha*Sum(A[i,k] * B[k,j]) + beta*C[i,j], k=0..n-1</tt>. <br>
     * Matrix shapes: <tt>A(m x n), B(n x p), C(m x p)</tt>. <br>
     * Note: Matrix shape conformance is checked <i>after</i> potential
     * transpositions.
     *
     * @param B
     *            the second source matrix.
     * @param C
     *            the matrix where results are to be stored. Set this parameter
     *            to <tt>null</tt> to indicate that a new result matrix shall be
     *            constructed.
     * @return C (for convenience only).
     *
     * @throws IllegalArgumentException
     *             if <tt>B.rows != A.columns</tt>.
     * @throws IllegalArgumentException
     *             if
     *             <tt>C.rows != A.rows || C.columns != B.columns</tt>.
     * @throws IllegalArgumentException
     *             if <tt>A == C || B == C</tt>.
     */
    def multiply(A: DoubleMatrix2D, B: DoubleMatrix2D, C: DoubleMatrix2D, alpha: Double, beta: Double): DoubleMatrix2D
  }

  trait MultiplyAlgorithmTranspose extends MultiplyAlgorithm {
    /**
     * Linear algebraic matrix-vector multiplication;
     * <tt>z = alpha * A * y + beta*z</tt>.
     * <tt>z[i] = alpha*Sum(A[i,j] * y[j]) + beta*z[i], i=0..A.rows-1, j=0..y.size()-1</tt>
     * . Where <tt>A == this</tt>. <br>
     * Note: Matrix shape conformance is checked <i>after</i> potential
     * transpositions.
     *
     * @param y
     *            the source vector.
     * @param z
     *            the vector where results are to be stored. Set this parameter
     *            to <tt>null</tt> to indicate that a new result vector shall be
     *            constructed.
     * @return z (for convenience only).
     *
     * @throws IllegalArgumentException
     *             if <tt>A.columns != y.size() || A.rows > z.size())</tt>.
     */
    def multiply(A: DoubleMatrix2D, y: DoubleMatrix1D, z: DoubleMatrix1D, alpha: Double, beta: Double, transposeA: Boolean): DoubleMatrix1D

    /**
     * Linear algebraic matrix-matrix multiplication;
     * <tt>C = alpha * A x B + beta*C</tt>.
     * <tt>C[i,j] = alpha*Sum(A[i,k] * B[k,j]) + beta*C[i,j], k=0..n-1</tt>. <br>
     * Matrix shapes: <tt>A(m x n), B(n x p), C(m x p)</tt>. <br>
     * Note: Matrix shape conformance is checked <i>after</i> potential
     * transpositions.
     *
     * @param B
     *            the second source matrix.
     * @param C
     *            the matrix where results are to be stored. Set this parameter
     *            to <tt>null</tt> to indicate that a new result matrix shall be
     *            constructed.
     * @return C (for convenience only).
     *
     * @throws IllegalArgumentException
     *             if <tt>B.rows != A.columns</tt>.
     * @throws IllegalArgumentException
     *             if
     *             <tt>C.rows != A.rows || C.columns != B.columns</tt>.
     * @throws IllegalArgumentException
     *             if <tt>A == C || B == C</tt>.
     */
    def multiply(A: DoubleMatrix2D, B: DoubleMatrix2D, C: DoubleMatrix2D, alpha: Double = 1.0, beta: Double = 0.0, transposeA: Boolean, transposeB: Boolean): DoubleMatrix2D
  }

  private var algorithmList = new ArrayBuffer[MultiplyAlgorithm]()
  algorithmList.append(new MultSimple)
  algorithmList.append(new MultNonZeroIterator)

  private var defaultAlgorithm: MultiplyAlgorithm = new MultNonZeroIterator

  def prependAlgorithm(a: MultiplyAlgorithm) {
    algorithmList.insert(0, a)
  }

  def appendAlgorithm(a: MultiplyAlgorithm) {
    algorithmList.append(a)
  }

  def setAlgorithmList(list: ArrayBuffer[MultiplyAlgorithm]) {
    algorithmList = list
  }

  def getAlgorithmList = algorithmList

  def setDefaultAlgorithm(a: MultiplyAlgorithm) {
    defaultAlgorithm = a
  }

  def getDefaultAlgorithm = defaultAlgorithm

  /**
   * Linear algebraic matrix-matrix multiplication; <tt>result = A x B</tt>;
   */
  def multiply(A: DoubleMatrix2D, B: DoubleMatrix2D): DoubleMatrix2D = {
    multiply2D(A, B, null)
  }

  /**
   * Linear algebraic matrix-matrix multiplication; <tt>result = A x B</tt>;
   */
  def multiply(A: DoubleMatrix2D, B: DoubleMatrix1D): DoubleMatrix1D = {
    multiply1D(A, B, null)
  }

  /**
   * Linear algebraic dot-product or vector-vector multiplication;
   * <tt>result = A * B</tt>.
   * <tt>result = Sum(A[i] * B[i]), i=0..A.size()-1</tt>
   * Note: Matrix shape conformance is checked.
   *
   * @param A
   *            the source vector.
   * @param B
   *            the source vector.
   * @return The dot product of the vectors
   *
   * @throws IllegalArgumentException
   *             if <tt>A.size() != y.size()</tt>.
   */
  def dot(A: DoubleMatrix1D, B: DoubleMatrix1D): Double = {
    check1DMult(A, B)

    algorithmList.foreach(a => {
      if (a.predicate(A, B))
        a.dot(A, B)
    })
    defaultAlgorithm.dot(A, B)
  }

  /**
   * Linear algebraic dot-product or vector-vector multiplication;
   * <tt>result = A * B</tt>.
   * <tt>result = Sum(A[i] * B[i]), i=start..end-1</tt>
   * Note: Matrix shape conformance is checked.
   *
   * @param A
   *            the source vector.
   * @param B
   *            the source vector.
   * @return The dot product of the vectors
   *
   * @throws IllegalArgumentException
   *             if <tt>A.size() != y.size()</tt>.
   */
  def dot(A: DoubleMatrix1D, B: DoubleMatrix1D, start: Int, end: Int): Double = {
    A.checkIndex(start)
    A.checkIndex(end-1)
    B.checkIndex(start)
    B.checkIndex(end-1)

    algorithmList.foreach(a => {
      if (a.predicate(A, B))
        a.dot(A, B)
    })
    defaultAlgorithm.dot(A, B, start, end)
  }

  /**
   * Linear algebraic matrix-vector multiplication;
   * <tt>z = alpha * A * y + beta*z</tt>.
   * <tt>z[i] = alpha*Sum(A[i,j] * y[j]) + beta*z[i], i=0..A.rows-1, j=0..y.size()-1</tt>
   * . Where <tt>A == this</tt>. <br>
   * Note: Matrix shape conformance is checked <i>after</i> potential
   * transpositions.
   *
   * @param y
   *            the source vector.
   * @param z
   *            the vector where results are to be stored. Set this parameter
   *            to <tt>null</tt> to indicate that a new result vector shall be
   *            constructed.
   * @return z (for convenience only).
   *
   * @throws IllegalArgumentException
   *             if <tt>A.columns != y.size() || A.rows > z.size())</tt>.
   */
  def multiply1D(A: DoubleMatrix2D, y: DoubleMatrix1D, z: DoubleMatrix1D, alpha: Double = 1.0, beta: Double = 0.0, transposeA: Boolean = false): DoubleMatrix1D = {
    check1DMult(A, y, z, transposeA)

    algorithmList.foreach(a => {
      if (a.predicate(A, y, z, transposeA)) {
        if (transposeA) {
          a match {
            case a_t: MultiplyAlgorithmTranspose => return a_t.multiply(A, y, z, alpha, beta, transposeA)
            case _ => {
              val AA = A.viewTranspose()
              return a.multiply(AA, y, z, alpha, beta)
            }
          }
        }
        else
          return a.multiply(A, y, z, alpha, beta)
      }
    })
    if (transposeA) {
      defaultAlgorithm match {
        case a_t: MultiplyAlgorithmTranspose => a_t.multiply(A, y, z, alpha, beta, transposeA)
        case _ => {
          val AA = A.viewTranspose()
          defaultAlgorithm.multiply(AA, y, z, alpha, beta)
        }
      }
    }
    else
      defaultAlgorithm.multiply(A, y, z, alpha, beta)
  }

  /**
   * Linear algebraic matrix-matrix multiplication;
   * <tt>C = alpha * A x B + beta*C</tt>.
   * <tt>C[i,j] = alpha*Sum(A[i,k] * B[k,j]) + beta*C[i,j], k=0..n-1</tt>. <br>
   * Matrix shapes: <tt>A(m x n), B(n x p), C(m x p)</tt>. <br>
   * Note: Matrix shape conformance is checked <i>after</i> potential
   * transpositions.
   *
   * @param B
   *            the second source matrix.
   * @param C
   *            the matrix where results are to be stored. Set this parameter
   *            to <tt>null</tt> to indicate that a new result matrix shall be
   *            constructed.
   * @return C (for convenience only).
   *
   * @throws IllegalArgumentException
   *             if <tt>B.rows != A.columns</tt>.
   * @throws IllegalArgumentException
   *             if
   *             <tt>C.rows != A.rows || C.columns != B.columns</tt>.
   * @throws IllegalArgumentException
   *             if <tt>A == C || B == C</tt>.
   */
  def multiply2D(A: DoubleMatrix2D, B: DoubleMatrix2D, C: DoubleMatrix2D, alpha: Double = 1.0, beta: Double = 0.0, transposeA: Boolean = false, transposeB: Boolean = false): DoubleMatrix2D = {
    check2DMult(A, B, C, transposeA, transposeB)

    algorithmList.foreach(a => {
      if (a.predicate(A, B, C, transposeA, transposeB)) {
        if (transposeA || transposeB) {
          a match {
            case a_t: MultiplyAlgorithmTranspose => return a_t.multiply(A, B, C, alpha, beta, transposeA, transposeB)
            case _ => {
              val AA = if (transposeA) A.viewTranspose() else A
              val BB = if (transposeB) B.viewTranspose() else B
              return a.multiply(AA, BB, C, alpha, beta)
            }
          }
        }
        else
          return a.multiply(A, B, C, alpha, beta)
      }
    })
    if (transposeA) {
      defaultAlgorithm match {
        case a_t: MultiplyAlgorithmTranspose => a_t.multiply(A, B, C, alpha, beta, transposeA, transposeB)
        case _ => {
          val AA = if (transposeA) A.viewTranspose() else A
          val BB = if (transposeB) B.viewTranspose() else B
          defaultAlgorithm.multiply(AA, BB, C, alpha, beta)
        }
      }
    }
    else
      defaultAlgorithm.multiply(A, B, C, alpha, beta)
  }

  def check1DMult(A: DoubleMatrix1D, B: DoubleMatrix1D) {
    checkSizesEqual(A, B)
  }

  def check1DMult(A: DoubleMatrix2D, y: DoubleMatrix1D, result: DoubleMatrix1D, transposeA: Boolean) {
    if (transposeA) {
      if (result != null)
        checkColumnsEqualsSize(A, result)
      checkRowsEqualsSize(A, y)
    }
    else {
      if (result != null)
        checkRowsEqualsSize(A, result)
      checkColumnsEqualsSize(A, y)
    }
    if (y eq result)
        throw new IllegalArgumentException("Source and destination matrices must not be the same y=" + System.identityHashCode(y) + " and result=" + System.identityHashCode(result))
  }

  def check2DMult(A: DoubleMatrix2D, B: DoubleMatrix2D, result: DoubleMatrix2D, transposeA: Boolean, transposeB: Boolean) {
    if (transposeA && transposeB)
      checkColumnsEqualsRows(B, A)
    else if (transposeA)
      checkRowsEqual(A, B)
    else if (transposeB)
      checkColumnsEqual(A, B)
    else
      checkColumnsEqualsRows(A, B)
    if (result != null) {
      if (transposeA)
        checkColumnsEqualsRows(A, result)
      else
        checkRowsEqual(A, result)
      if (transposeB)
        checkColumnsEqualsRows(result, B)
      else
        checkColumnsEqual(B, result)
    }
    if (A eq result)
        throw new IllegalArgumentException("Source and destination matrices must not be the same A=" + System.identityHashCode(A) + " and result=" + System.identityHashCode(result))
    if (B eq result)
        throw new IllegalArgumentException("Source and destination matrices must not be the same B=" + System.identityHashCode(B) + " and result=" + System.identityHashCode(result))
  }

  class MultSimple extends MultiplyAlgorithm {

    override def predicate(A: DoubleMatrix1D, B: DoubleMatrix1D) = {
      ! A.isSparse && ! B.isSparse && ! A.isView && ! B.isView
    }

    override def predicate(A: DoubleMatrix2D, B: DoubleMatrix1D, C: DoubleMatrix1D, transposeA: Boolean) = {
      ! transposeA && ! A.isSparse && ! B.isSparse && ! A.isView && ! B.isView
    }

    override def predicate(A: DoubleMatrix2D, B: DoubleMatrix2D, C: DoubleMatrix2D, transposeA: Boolean, transposeB: Boolean) = {
      ! transposeA && ! transposeB && ! A.isSparse && ! B.isSparse && ! A.isView && ! B.isView
    }

    def dot(A: DoubleMatrix1D, B: DoubleMatrix1D): Double = {
      var sum = 0.0
      for (idx <- 0 until A.size.toInt) {
         sum += A.getQuick(idx) * B.getQuick(idx)
      }
      sum
    }

    def dot(A: DoubleMatrix1D, B: DoubleMatrix1D, start: Int, end: Int): Double = {
      var sum = 0.0
      for (idx <- start until end) {
         sum += A.getQuick(idx) * B.getQuick(idx)
      }
      sum
    }

    def multiply(A: DoubleMatrix2D, y: DoubleMatrix1D, z: DoubleMatrix1D, alpha: Double, beta_p: Double): DoubleMatrix1D = {
      val beta = if (z == null) 0.0 else beta_p
      val zz: DoubleMatrix1D = if (z == null) y.like1D(A.rows) else z

      MatrixProcessor.singleton.processRows[Double](A, zz, new Function1[Int, Double]() {
        def apply(rowIdx: Int) = {
          var s = 0.0
          for (colIdx <- 0 until A.columns) {
              s += A.getQuick(rowIdx, colIdx) * y.getQuick(colIdx)
          }
          alpha * s + beta * zz.getQuick(rowIdx)
        }
      })
      zz
    }

    def multiply(A: DoubleMatrix2D, B: DoubleMatrix2D, C: DoubleMatrix2D, alpha: Double, beta_p: Double): DoubleMatrix2D = {
      val beta = if (C == null) 0.0 else beta_p
      val CC = if (C == null) A.like2D(A.rows, B.columns) else C

      MatrixProcessor.singleton.processIndexes(B.columns, new IntProcedure(){
        def apply(bColIdx: Int) = {
          for (aRowIdx <- 0 until A.rows) {
              var sum = 0.0
              for (commonIdx <- 0 until A.columns) {
                  sum += A.getQuick(aRowIdx, commonIdx) * B.getQuick(commonIdx, bColIdx)
              }
            if (beta == 0.0)
              CC.setQuick(aRowIdx, bColIdx, alpha * sum)
            else
              CC.setQuick(aRowIdx, bColIdx, alpha * sum + beta * CC.getQuick(aRowIdx, bColIdx))
          }
          true
        }
      }, parallelAllowed = MatrixProcessor.singleton.canWriteInParallel(CC))
      CC
    }
  }

  class MultForEachNonZero extends MultiplyAlgorithm {

    def dot(A: DoubleMatrix1D, B: DoubleMatrix1D): Double = {
      var sum = 0.0
      A.forEachNonZero(new Function2[Int, Double, Double]() {
            def apply(idx: Int, value: Double) = {
              sum += value * B.getQuick(idx)
              value
            }
          })
      sum
    }

    def dot(A: DoubleMatrix1D, B: DoubleMatrix1D, start: Int, end: Int): Double = {
      var sum = 0.0
      A.forEachNonZero(new Function2[Int, Double, Double]() {
            def apply(idx: Int, value: Double) = {
              if (idx >= start && idx < end)
                sum += value * B.getQuick(idx)
              value
            }
          })
      sum
    }

    def multiply(A: DoubleMatrix2D, y: DoubleMatrix1D, z: DoubleMatrix1D, alpha: Double, beta_p: Double): DoubleMatrix1D = {
      val beta = if (z == null) 0.0 else beta_p
      val zz: DoubleMatrix1D = if (z == null) y.like1D(A.rows) else z

      MatrixProcessor.singleton.processRows[Double](A, zz, new Function1[Int, Double]() {
        def apply(rowIdx: Int) = {
          var sum = 0.0
          A.forEachNonZeroInRow(rowIdx, new Function3[Int, Int, Double, Double]() {
                def apply(rowIdx: Int, colIdx: Int, value: Double) = {
                  sum += value * y.getQuick(colIdx)
                  value
                }
              })
          if (beta == 0.0)
            alpha * sum
          else
            alpha * sum + beta * zz.getQuick(rowIdx)
        }
      })
      zz
    }

    def multiply(A: DoubleMatrix2D, B: DoubleMatrix2D, C: DoubleMatrix2D, alpha: Double, beta_p: Double): DoubleMatrix2D = {
      val beta = if (C == null) 0.0 else beta_p
      val CC = if (C == null) A.like2D(A.rows, B.columns) else C

      MatrixProcessor.singleton.processIndexes(B.columns, new IntProcedure(){
        def apply(bColIdx: Int) = {
          for (aRowIdx <- 0 until A.rows) {
            var sum = 0.0
            A.forEachNonZeroInRow(aRowIdx, new Function3[Int, Int, Double, Double]() {
              def apply(rowIdx: Int, commonIdx: Int, value: Double) = {
                sum += value * B.getQuick(commonIdx, bColIdx)
                value
              }
            })
            if (beta == 0.0)
              CC.setQuick(aRowIdx, bColIdx, alpha * sum)
            else
              CC.setQuick(aRowIdx, bColIdx, alpha * sum + beta * CC.getQuick(aRowIdx, bColIdx))
          }
          true
        }
      }, parallelAllowed = MatrixProcessor.singleton.canWriteInParallel(CC))
      CC
    }
  }

  class MultNonZeroIterator extends MultiplyAlgorithm {

    @inline
    def compareColumnToRow[T](iter1: IndexIterator2D[T], iter2: IndexIterator2D[T]): Int = {
      if (iter1.column < iter2.row)
        -1
      else if (iter1.column > iter2.row)
        1
      else
        0
    }

    @inline
    def compareColumnToIndex[T](iter1: IndexIterator2D[T], iter2: IndexIterator1D[T]): Int = {
      if (iter1.column < iter2.index)
        -1
      else if (iter1.column > iter2.index)
        1
      else
        0
    }

    @inline
    def compareIndexToIndex[T](iter1: IndexIterator1D[T], iter2: IndexIterator1D[T]): Int = {
      if (iter1.index < iter2.index)
        -1
      else if (iter1.index > iter2.index)
        1
      else
        0
    }

    def dot(A: DoubleMatrix1D, B: DoubleMatrix1D): Double = {
      val iterA = A.iteratorNonZeros
      val iterB = B.iteratorNonZeros
      var sum = 0.0
      while(iterA.hasValue || iterB.hasValue) {
        compareIndexToIndex(iterA, iterB) match {
          case -1 => iterA.increment()
          case 1 => iterB.increment()
          case 0 => {
            sum += iterA.value * iterB.value
            iterA.increment()
            iterB.increment()
          }
        }
      }
      sum
    }

    def dot(A: DoubleMatrix1D, B: DoubleMatrix1D, start: Int, end: Int): Double = {
      val iterA = A.iteratorNonZeros
      val iterB = B.iteratorNonZeros
      var sum = 0.0
      while(iterA.hasValue || iterB.hasValue) {
        compareIndexToIndex(iterA, iterB) match {
          case -1 => iterA.increment()
          case 1 => iterB.increment()
          case 0 => {
            if (iterA.index >= start && iterA.index < end)
              sum += iterA.value * iterB.value
            iterA.increment()
            iterB.increment()
          }
        }
      }
      sum
    }

    def multiply(A: DoubleMatrix2D, B: DoubleMatrix1D, C: DoubleMatrix1D, alpha: Double, beta_p: Double): DoubleMatrix1D = {
      val beta = if (C == null) 0.0 else beta_p
      val CC = if (C == null) A.like1D(A.rows) else C

      MatrixProcessor.singleton.processCellIndexes(CC, new Function1[Int, Double]() {
        def apply(rowIdx: Int): Double = {
          val iterA = A.iteratorNonZerosInRow(rowIdx)
          val iterB = B.iteratorNonZeros
          var sum = 0.0
          while(iterA.hasValue || iterB.hasValue) {
            compareColumnToIndex(iterA, iterB) match {
              case -1 => iterA.increment()
              case 1 => iterB.increment()
              case 0 => {
                sum += iterA.value * iterB.value
                iterA.increment()
                iterB.increment()
              }
            }
          }
          if (beta != 0.0)
            sum * alpha + beta * CC.getQuick(rowIdx)
          else
            sum * alpha
        }
      })
      CC
    }

    def multiply(A: DoubleMatrix2D, B: DoubleMatrix2D, C: DoubleMatrix2D, alpha: Double, beta_p: Double): DoubleMatrix2D = {
      val beta = if (C == null) 0.0 else beta_p
      val CC = if (C == null) A.like2D(A.rows, B.columns) else C

      MatrixProcessor.singleton.processCells[Double](CC, new Function2[Int, Int, Double]() {
        def apply(rowIdx: Int, colIdx: Int): Double = {
          val iterA = A.iteratorNonZerosInRow(rowIdx)
          val iterB = B.iteratorNonZerosInColumn(colIdx)
          var sum = 0.0
          while(iterA.hasValue || iterB.hasValue) {
            compareColumnToRow(iterA, iterB) match {
              case -1 => iterA.increment()
              case 1 => iterB.increment()
              case 0 => {
                sum += iterA.value * iterB.value
                iterA.increment()
                iterB.increment()
              }
            }
          }
          if (beta != 0.0)
            sum * alpha + beta * CC.getQuick(rowIdx, colIdx)
          else
            sum * alpha
        }
      })
      CC
    }
  }

/*
  //TODO: SparseRC2D
  def multSparseRowMajor(A: SparseRCDoubleMatrix2D, y: DoubleMatrix1D, z: DoubleMatrix1D, alpha: Double, beta: Double, transposeA: Boolean = false): DoubleMatrix1D = {
    if ( ! y.isInstanceOf[DenseMatrix1D] || (z != null && ! z.isInstanceOf[DenseMatrix1D])) {
      return multStandard(A, y, z, alpha, beta, transposeA)
    }

    val rowsA = if (transposeA) A.columns else A.rows
    val columnsA = if (transposeA) A.rows else A.columns
    checkColumnsEqualsSize(columnsA, y)
    checkRowsEqualsSize(rowsA, z)

    val zz = if (z == null) new DenseMatrix1D[Double](rowsA) else z.asInstanceOf[DenseMatrix1D[Double]]
    val yy = y.asInstanceOf[DenseMatrix1D[Double]]
     val elementsZ: Array[Double] = zz.elements
     val strideZ = zz.stride
     val zeroZ = zz.toRawIndex(0).toInt
     val elementsY = yy.elements
     val strideY = yy.stride
     val zeroY = yy.toRawIndex(0).toInt
     if (transposeA) {
       if (z != null && beta != 1.0) z.assign(DoubleFunctions.mult(beta))
       for (aRowIdx <- 0 until A.rows) {
           val high = A.rowPointers(aRowIdx + 1)
           val yElem = alpha * elementsY(zeroY + strideY * aRowIdx)
           for (k <- A.rowPointers(aRowIdx) until high) {
             elementsZ(zeroZ + strideZ * A.columnIndexes(k)) += A.values(k) * yElem
           }
       }
     }
     else {
       var zidx = zeroZ
       var k = A.rowPointers(0)
       for (rowIdx <- 0 until A.rows) {
         var sum = 0.0
         val high = A.rowPointers(rowIdx + 1)
         while (k + 10 < high) {
           val ind = k + 9
           sum += A.values(ind) * elementsY(zeroY + strideY * A.columnIndexes(ind)) +
                  A.values(ind-1) * elementsY(zeroY + strideY * A.columnIndexes(ind-1)) +
                  A.values(ind-2) * elementsY(zeroY + strideY * A.columnIndexes(ind-2)) +
                  A.values(ind-3) * elementsY(zeroY + strideY * A.columnIndexes(ind-3)) +
                  A.values(ind-4) * elementsY(zeroY + strideY * A.columnIndexes(ind-4)) +
                  A.values(ind-5) * elementsY(zeroY + strideY * A.columnIndexes(ind-5)) +
                  A.values(ind-6) * elementsY(zeroY + strideY * A.columnIndexes(ind-6)) +
                  A.values(ind-7) * elementsY(zeroY + strideY * A.columnIndexes(ind-7)) +
                  A.values(ind-8) * elementsY(zeroY + strideY * A.columnIndexes(ind-8)) +
                  A.values(ind-9) * elementsY(zeroY + strideY * A.columnIndexes(ind-9))
           k += 10
         }
         while (k < high) {
           sum += A.values(k) * elementsY(A.columnIndexes(k))
           k += 1
         }
         if (beta == 0.0)
           elementsZ(zidx) = alpha * sum
         else
           elementsZ(zidx) = alpha * sum + beta * elementsZ(zidx)
         zidx += strideZ
       }
     }
     z
   }

  //TODO: SparseRC2D
  def zMult(A: SparseRCDoubleMatrix2D, B: DoubleMatrix2D, C_p: DoubleMatrix2D, alpha: Double, beta: Double, transposeA: Boolean, transposeB: Boolean): DoubleMatrix2D = {
    var rowsA = if (transposeA) A.columns else A.rows
    var columnsA = if (transposeA) A.rows else A.columns
    var rowsB = if (transposeB) B.columns else B.rows
    var columnsB = if (transposeB) B.rows else B.columns
    val p = columnsB

    val C =
      if (C_p != null)
        C_p
      else if (B.isInstanceOf[SparseRCDoubleMatrix2D])
        new SparseRCDoubleMatrix2D(rowsA, p, rowsA * p)
      else
        new DenseMatrix2D(rowsA, p)


    checkColumnsEqualsRows(if (transposeA) A.viewTranspose() else A, if (transposeB) B.viewTranspose() else B)
    checkRowsEqual(if (transposeA) A.viewTranspose() else A, C)
    checkColumnsEqual(C, if (transposeB) B.viewTranspose() else B)

    if (A == C || B == C)
        throw new IllegalArgumentException("Source and destination matrices must not be the same A=" + System.identityHashCode(A) + ", B=" + System.identityHashCode(B) + ", result=" + System.identityHashCode(C))
     if (C != null && beta != 1.0)
       C.assign(DoubleFunctions.mult(beta))

     if (B.isInstanceOf[DenseMatrix2D] && C.isInstanceOf[DenseMatrix2D]) {
       var AA = if (transposeA) A.getTranspose else A
       var BB: DenseMatrix2D = null
       BB = if (transposeB) B.viewTranspose().asInstanceOf[DenseMatrix2D] else B.asInstanceOf[DenseMatrix2D]
       val CC = C.asInstanceOf[DenseMatrix2D]
       val rowPointersA = AA.rowPointers
       val columnIndexesA = AA.columnIndexes
       val valuesA = AA.values
       for (ii <- 0 until rowsA) {
         val highA = rowPointersA(ii + 1)
         for (ka <- rowPointersA(ii) until highA) {
           val scal = valuesA(ka) * alpha
           val jj = columnIndexesA(ka)
           CC.viewRow(ii).assign(BB.viewRow(jj), DoubleFunctions.plusMultSecond(scal))
         }
       }
     }
     else if (B.isInstanceOf[SparseRCDoubleMatrix2D] && C.isInstanceOf[SparseRCDoubleMatrix2D]) {
       val CC = C.asInstanceOf[SparseRCDoubleMatrix2D]
       val AA = if (transposeA) A.getTranspose else A
       val BB = if (transposeB) B.asInstanceOf[SparseRCDoubleMatrix2D].getTranspose else B.asInstanceOf[SparseRCDoubleMatrix2D]
       val nzmax = CC.numNonZero.toInt
       val iw = Array.ofDim[Int](columnsB + 1)
       for (i <- 0 until iw.length) {
         iw(i) = -1
       }
       var len = -1
       for (ii <- 0 until rowsA) {
         val highA = AA.rowPointers(ii + 1)
         for (ka <- AA.rowPointers(ii) until highA) {
           val scal = AA.values(ka) * alpha
           val jj = AA.columnIndexes(ka)
           val highB = BB.rowPointers(jj + 1)
           for (kb <- BB.rowPointers(jj) until highB) {
             val jcol = BB.columnIndexes(kb)
             val jpos = iw(jcol)
             if (jpos == -1) {
               len += 1
               if (len >= nzmax) {
                 throw new IllegalArgumentException("The max number of nonzero elements in C is too small.")
               }
               CC.columnIndexes(len) = jcol
               iw(jcol) = len
               CC.values(len) = scal * BB.values(kb)
             } else {
               CC.values(jpos) += scal * BB.values(kb)
             }
           }
         }
         for (k <- CC.rowPointers(ii) until len + 1) {
           iw(CC.columnIndexes(k)) = -1
         }
         CC.rowPointers(ii + 1) = len + 1
       }
     }
     else {
       val BB = if (transposeB) B.viewTranspose() else B
       val Brows = Array.ofDim[DoubleMatrix1D](columnsA)
       var i = columnsA
       while (i >= 0) Brows(i) = B.viewRow(i)
       val Crows = Array.ofDim[DoubleMatrix1D](rowsA)
       i = rowsA
       while (i >= 0) Crows(i) = C.viewRow(i)
       val fun = cern.jet.math.tdouble.DoublePlusMultSecond.plusMult(0)
       i = A.rows
       while (i >= 0) {
         val low = A.rowPointers(i)
         var k = A.rowPointers(i + 1)
         while (k >= low) {
           val j = A.columnIndexes(k)
           fun.multiplicator = A.values(k) * alpha
           if (!transposeA) Crows(i).assign(Brows(j), fun) else Crows(j).assign(Brows(i), fun)
         }
       }
     }
     C
   }

  //TODO: SparseHash2D
  def zMult(A: DoubleMatrix2D, y: DoubleMatrix1D,
      z: DoubleMatrix1D,
      alpha: Double,
      beta: Double,
      transposeA: Boolean): DoubleMatrix1D = {
    var rowsA = rows
    var columnsA = columns
    if (transposeA) {
      rowsA = columns
      columnsA = rows
    }
    val ignore = (z == null)
    if (z == null) z = new DenseMatrix1D[Double](rowsA)
    if (!(this.isNoView && y.isInstanceOf[DenseMatrix1D] &&
      z.isInstanceOf[DenseMatrix1D])) {
      return super.zMult(y, z, alpha, beta, transposeA)
    }
    if (columnsA != y.size || rowsA > z.size) throw new IllegalArgumentException("Incompatible args: " +
      ((if (transposeA) viewTranspose() else this).toShapeString()) +
      ", " +
      y.toShapeString() +
      ", " +
      z.toShapeString())
    if (!ignore) z.assign(cern.jet.math.tdouble.DoubleFunctions.mult(beta))
    val zz = z.asInstanceOf[DenseMatrix1D]
    val elementsZ = zz.elements
    val strideZ = zz.stride()
    val zeroZ = z.index(0).toInt
    val yy = y.asInstanceOf[DenseMatrix1D]
    val elementsY = yy.elements
    val strideY = yy.stride()
    val zeroY = y.index(0).toInt
    if (elementsY == null || elementsZ == null) throw new InternalError()
    this.elements.forEachPair(new cern.colt.function.tdouble.LongDoubleProcedure() {

      def apply(key: Long, value: Double): Boolean = {
        var i = (key / columns).toInt
        var j = (key % columns).toInt
        if (transposeA) {
          val tmp = i
          i = j
          j = tmp
        }
        elementsZ(zeroZ + strideZ * i) += alpha * value * elementsY(zeroY + strideY * j)
        return true
      }
    })
    z
  }

  //TODO: SparseHash2D/
  def zMult(A: DoubleMatrix2D, B: DoubleMatrix2D,
      C: DoubleMatrix2D,
      alpha: Double,
      beta: Double,
      transposeA: Boolean,
      transposeB: Boolean): DoubleMatrix2D = {
    if (!(this.isNoView)) {
      return super.zMult(B, C, alpha, beta, transposeA, transposeB)
    }
    if (transposeB) B = B.viewTranspose()
    var rowsA = rows
    var columnsA = columns
    if (transposeA) {
      rowsA = columns
      columnsA = rows
    }
    val p = B.columns
    val ignore = (C == null)
    if (C == null) C = new DenseMatrix2D(rowsA, p)
    if (B.rows != columnsA) throw new IllegalArgumentException("Matrix2D inner dimensions must agree:" + toShapeString() +
      ", " +
      (if (transposeB) B.viewTranspose() else B).toShapeString())
    if (C.rows != rowsA || C.columns != p) throw new IllegalArgumentException("Incompatibel result matrix: " + toShapeString() + ", " +
      (if (transposeB) B.viewTranspose() else B).toShapeString() +
      ", " +
      C.toShapeString())
    if (this eq C || B eq C) throw new IllegalArgumentException("Matrices must not be identical")
    if (!ignore) C.assign(cern.jet.math.tdouble.DoubleFunctions.mult(beta))
    val Brows = Array.ofDim[DoubleMatrix1D](columnsA)
    var i = columnsA
    while (i >= 0) Brows(i) = B.viewRow(i)
    val Crows = Array.ofDim[DoubleMatrix1D](rowsA)
    var i = rowsA
    while (i >= 0) Crows(i) = C.viewRow(i)
    val fun = cern.jet.math.tdouble.DoublePlusMultSecond.plusMult(0)
    this.elements.forEachPair(new cern.colt.function.tdouble.LongDoubleProcedure() {

      def apply(key: Long, value: Double): Boolean = {
        val i = (key / columns).toInt
        val j = (key % columns).toInt
        fun.multiplicator = value * alpha
        if (!transposeA) Crows(i).assign(Brows(j), fun) else Crows(j).assign(Brows(i), fun)
        return true
      }
    })
    C
  }

  //TODO: SparceCC2D
  def zMult(A: DoubleMatrix2D, y: DoubleMatrix1D,
      z: DoubleMatrix1D,
      alpha: Double,
      beta: Double,
      transposeA: Boolean): DoubleMatrix1D = {
    val rowsA = if (transposeA) columns else rows
    val columnsA = if (transposeA) rows else columns
    val ignore = (z == null || transposeA)
    if (z == null) z = new DenseMatrix1D(rowsA)
    if (!(y.isInstanceOf[DenseMatrix1D] && z.isInstanceOf[DenseMatrix1D])) {
      return super.zMult(y, z, alpha, beta, transposeA)
    }
    if (columnsA != y.size || rowsA > z.size) throw new IllegalArgumentException("Incompatible args: " +
      ((if (transposeA) viewTranspose() else this).toShapeString()) +
      ", " +
      y.toShapeString() +
      ", " +
      z.toShapeString())
    val zz = z.asInstanceOf[DenseMatrix1D]
    val elementsZ = zz.elements
    val strideZ = zz.stride()
    val zeroZ = zz.index(0).toInt
    val yy = y.asInstanceOf[DenseMatrix1D]
    val elementsY = yy.elements
    val strideY = yy.stride()
    val zeroY = yy.index(0).toInt
    val rowIndexesA = dcs.i
    val columnPointersA = dcs.p
    val valuesA = dcs.x
    var zidx = zeroZ
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if (!transposeA) {
      if ((!ignore) && (beta / alpha != 1.0)) {
        z.assign(cern.jet.math.tdouble.DoubleFunctions.mult(beta / alpha))
      }
      if ((nthreads > 1) &&
        (cardinality() >= ConcurrencyUtils.getThreadsBeginN_2D)) {
        nthreads = 2
        val futures = Array.ofDim[Future](nthreads)
        val result = Array.ofDim[Double](rowsA)
        val k = columns / nthreads
        for (j <- 0 until nthreads) {
          val firstColumn = j * k
          val lastColumn = if ((j == nthreads - 1)) columns else firstColumn + k
          val threadID = j
          futures(j) = ConcurrencyUtils.submit(new Runnable() {

            def run() {
              if (threadID == 0) {
                for (i <- firstColumn until lastColumn) {
                  var high = columnPointersA(i + 1)
                  var yElem = elementsY(zeroY + strideY * i)
                  for (k <- columnPointersA(i) until high) {
                    var j = rowIndexesA(k)
                    elementsZ(zeroZ + strideZ * j) += valuesA(k) * yElem
                  }
                }
              } else {
                for (i <- firstColumn until lastColumn) {
                  var high = columnPointersA(i + 1)
                  var yElem = elementsY(zeroY + strideY * i)
                  for (k <- columnPointersA(i) until high) {
                    var j = rowIndexesA(k)
                    result(j) += valuesA(k) * yElem
                  }
                }
              }
            }
          })
        }
        ConcurrencyUtils.waitForCompletion(futures)
        val rem = rowsA % 10
        var j = rem
        while (j < rowsA) {
          elementsZ(zeroZ + j * strideZ) += result(j)
          elementsZ(zeroZ + (j + 1) * strideZ) += result(j + 1)
          elementsZ(zeroZ + (j + 2) * strideZ) += result(j + 2)
          elementsZ(zeroZ + (j + 3) * strideZ) += result(j + 3)
          elementsZ(zeroZ + (j + 4) * strideZ) += result(j + 4)
          elementsZ(zeroZ + (j + 5) * strideZ) += result(j + 5)
          elementsZ(zeroZ + (j + 6) * strideZ) += result(j + 6)
          elementsZ(zeroZ + (j + 7) * strideZ) += result(j + 7)
          elementsZ(zeroZ + (j + 8) * strideZ) += result(j + 8)
          elementsZ(zeroZ + (j + 9) * strideZ) += result(j + 9)
          j += 10
        }
        for (j <- 0 until rem) {
          elementsZ(zeroZ + j * strideZ) += result(j)
        }
      } else {
        for (i <- 0 until columns) {
          val high = columnPointersA(i + 1)
          val yElem = elementsY(zeroY + strideY * i)
          for (k <- columnPointersA(i) until high) {
            val j = rowIndexesA(k)
            elementsZ(zeroZ + strideZ * j) += valuesA(k) * yElem
          }
        }
      }
      if (alpha != 1.0) {
        z.assign(cern.jet.math.tdouble.DoubleFunctions.mult(alpha))
      }
    } else {
      if ((nthreads > 1) &&
        (cardinality() >= ConcurrencyUtils.getThreadsBeginN_2D)) {
        val futures = Array.ofDim[Future](nthreads)
        val k = columns / nthreads
        for (j <- 0 until nthreads) {
          val firstColumn = j * k
          val lastColumn = if ((j == nthreads - 1)) columns else firstColumn + k
          futures(j) = ConcurrencyUtils.submit(new Runnable() {

            def run() {
              var zidx = zeroZ + firstColumn * strideZ
              var k = dcs.p(firstColumn)
              for (i <- firstColumn until lastColumn) {
                var sum = 0
                var high = dcs.p(i + 1)
                while (k + 10 < high) {
                  var ind = k + 9
                  sum += valuesA(ind) * elementsY(zeroY + strideY * dcs.i(ind -= 1)) +
                    valuesA(ind) * elementsY(zeroY + strideY * dcs.i(ind -= 1)) +
                    valuesA(ind) * elementsY(zeroY + strideY * dcs.i(ind -= 1)) +
                    valuesA(ind) * elementsY(zeroY + strideY * dcs.i(ind -= 1)) +
                    valuesA(ind) * elementsY(zeroY + strideY * dcs.i(ind -= 1)) +
                    valuesA(ind) * elementsY(zeroY + strideY * dcs.i(ind -= 1)) +
                    valuesA(ind) * elementsY(zeroY + strideY * dcs.i(ind -= 1)) +
                    valuesA(ind) * elementsY(zeroY + strideY * dcs.i(ind -= 1)) +
                    valuesA(ind) * elementsY(zeroY + strideY * dcs.i(ind -= 1)) +
                    valuesA(ind) * elementsY(zeroY + strideY * dcs.i(ind -= 1))
                  k += 10
                }
                while (k < high) {
                  sum += valuesA(k) * elementsY(dcs.i(k))
                  k += 1
                }
                elementsZ(zidx) = alpha * sum + beta * elementsZ(zidx)
                zidx += strideZ
              }
            }
          })
        }
        ConcurrencyUtils.waitForCompletion(futures)
      } else {
        var k = dcs.p(0)
        for (i <- 0 until columns) {
          var sum = 0
          val high = dcs.p(i + 1)
          while (k + 10 < high) {
            val ind = k + 9
            sum += valuesA(ind) * elementsY(zeroY + strideY * dcs.i(ind -= 1)) +
              valuesA(ind) * elementsY(zeroY + strideY * dcs.i(ind -= 1)) +
              valuesA(ind) * elementsY(zeroY + strideY * dcs.i(ind -= 1)) +
              valuesA(ind) * elementsY(zeroY + strideY * dcs.i(ind -= 1)) +
              valuesA(ind) * elementsY(zeroY + strideY * dcs.i(ind -= 1)) +
              valuesA(ind) * elementsY(zeroY + strideY * dcs.i(ind -= 1)) +
              valuesA(ind) * elementsY(zeroY + strideY * dcs.i(ind -= 1)) +
              valuesA(ind) * elementsY(zeroY + strideY * dcs.i(ind -= 1)) +
              valuesA(ind) * elementsY(zeroY + strideY * dcs.i(ind -= 1)) +
              valuesA(ind) * elementsY(zeroY + strideY * dcs.i(ind -= 1))
            k += 10
          }
          while (k < high) {
            sum += valuesA(k) * elementsY(dcs.i(k))
            k += 1
          }
          elementsZ(zidx) = alpha * sum + beta * elementsZ(zidx)
          zidx += strideZ
        }
      }
    }
    z
  }

  //TODO: SparceCC2D
  def zMult(A: DoubleMatrix2D, B: DoubleMatrix2D,
      C: DoubleMatrix2D,
      alpha: Double,
      beta: Double,
      transposeA: Boolean,
      transposeB: Boolean): DoubleMatrix2D = {
    var rowsA = rows
    var columnsA = columns
    if (transposeA) {
      rowsA = columns
      columnsA = rows
    }
    var rowsB = B.rows
    var columnsB = B.columns
    if (transposeB) {
      rowsB = B.columns
      columnsB = B.rows
    }
    val p = columnsB
    val ignore = (C == null)
    if (C == null) {
      C = if (B.isInstanceOf[SparseCCDoubleMatrix2D]) new SparseCCDoubleMatrix2D(rowsA, p, (rowsA * p)) else new DenseMatrix2D(rowsA,
        p)
    }
    if (rowsB != columnsA) throw new IllegalArgumentException("Matrix2D inner dimensions must agree:" + toShapeString() +
      ", " +
      (if (transposeB) B.viewTranspose() else B).toShapeString())
    if (C.rows != rowsA || C.columns != p) throw new IllegalArgumentException("Incompatible result matrix: " + toShapeString() + ", " +
      (if (transposeB) B.viewTranspose() else B).toShapeString() +
      ", " +
      C.toShapeString())
    if (this eq C || B eq C) throw new IllegalArgumentException("Matrices must not be identical")
    if (!ignore && beta != 1.0) {
      C.assign(cern.jet.math.tdouble.DoubleFunctions.mult(beta))
    }
    if ((B.isInstanceOf[DenseMatrix2D]) && (C.isInstanceOf[DenseMatrix2D])) {
      var AA: SparseCCDoubleMatrix2D = null
      AA = if (transposeA) getTranspose else this
      var BB: DenseMatrix2D = null
      BB = if (transposeB) B.viewTranspose().asInstanceOf[DenseMatrix2D] else B.asInstanceOf[DenseMatrix2D]
      val CC = C.asInstanceOf[DenseMatrix2D]
      val columnPointersA = AA.dcs.p
      val rowIndexesA = AA.dcs.i
      val valuesA = AA.dcs.x
      val zeroB = BB.index(0, 0).toInt
      val rowStrideB = BB.rowStride()
      val columnStrideB = BB.columnStride()
      val elementsB = BB.elements
      val zeroC = CC.index(0, 0).toInt
      val rowStrideC = CC.rowStride()
      val columnStrideC = CC.columnStride()
      val elementsC = CC.elements
      for (jj <- 0 until columnsB; kk <- 0 until columnsA) {
        val high = columnPointersA(kk + 1)
        val yElem = elementsB(zeroB + kk * rowStrideB + jj * columnStrideB)
        for (ii <- columnPointersA(kk) until high) {
          val j = rowIndexesA(ii)
          elementsC(zeroC + j * rowStrideC + jj * columnStrideC) += valuesA(ii) * yElem
        }
      }
      if (alpha != 1.0) {
        C.assign(cern.jet.math.tdouble.DoubleFunctions.mult(alpha))
      }
    } else if ((B.isInstanceOf[SparseCCDoubleMatrix2D]) && (C.isInstanceOf[SparseCCDoubleMatrix2D])) {
      var AA: SparseCCDoubleMatrix2D = null
      AA = if (transposeA) getTranspose else this
      var BB = B.asInstanceOf[SparseCCDoubleMatrix2D]
      if (transposeB) {
        BB = BB.getTranspose
      }
      val CC = C.asInstanceOf[SparseCCDoubleMatrix2D]
      CC.dcs = Dcs_multiply.cs_multiply(AA.dcs, BB.dcs)
      if (CC.dcs == null) {
        throw new IllegalArgumentException("Exception occured in cs_multiply()")
      }
      if (alpha != 1.0) {
        CC.assign(cern.jet.math.tdouble.DoubleFunctions.mult(alpha))
      }
    } else {
      if (transposeB) {
        B = B.viewTranspose()
      }
      val Brows = Array.ofDim[DoubleMatrix1D](columnsA)
      var i = columnsA
      while (i >= 0) Brows(i) = B.viewRow(i)
      val Crows = Array.ofDim[DoubleMatrix1D](rowsA)
      var i = rowsA
      while (i >= 0) Crows(i) = C.viewRow(i)
      val fun = cern.jet.math.tdouble.DoublePlusMultSecond.plusMult(0)
      val rowIndexesA = dcs.i
      val columnPointersA = dcs.p
      val valuesA = dcs.x
      var i = columns
      while (i >= 0) {
        val low = columnPointersA(i)
        var k = columnPointersA(i + 1)
        while (k >= low) {
          val j = rowIndexesA(k)
          fun.multiplicator = valuesA(k) * alpha
          if (!transposeA) Crows(j).assign(Brows(i), fun) else Crows(i).assign(Brows(j), fun)
        }
      }
    }
    C
  }

  //TODO: Diagonal2D
  def zMult(A: DoubleMatrix2D, y: DoubleMatrix1D,
      z: DoubleMatrix1D,
      alpha: Double,
      beta: Double,
      transposeA: Boolean): DoubleMatrix1D = {
    var rowsA = rows
    var columnsA = columns
    if (transposeA) {
      rowsA = columns
      columnsA = rows
    }
    val ignore = z == null
    if (z == null) z = new DenseMatrix1D(rowsA)
    if (!(this.isNoView && y.isInstanceOf[DenseMatrix1D] &&
      z.isInstanceOf[DenseMatrix1D])) {
      return super.zMult(y, z, alpha, beta, transposeA)
    }
    if (columnsA != y.size || rowsA > z.size) throw new IllegalArgumentException("Incompatible args: " +
      (if (transposeA) viewTranspose() else this).toShapeString()) +
      ", " +
      y.toShapeString() +
      ", " +
      z.toShapeString())
    if ((!ignore) && ((beta) != 1)) z.assign(cern.jet.math.tdouble.DoubleFunctions.mult(beta))
    val zz = z.asInstanceOf[DenseMatrix1D]
    val elementsZ = zz.elements
    val strideZ = zz.stride()
    val zeroZ = z.index(0).toInt
    val yy = y.asInstanceOf[DenseMatrix1D]
    val elementsY = yy.elements
    val strideY = yy.stride()
    val zeroY = y.index(0).toInt
    if (elementsY == null || elementsZ == null) throw new InternalError()
    if (!transposeA) {
      if (dindex >= 0) {
        var i = dlength
        while (i >= 0) {
          elementsZ(zeroZ + strideZ * i) += alpha * elements(i) * elementsY(dindex + zeroY + strideY * i)
        }
      } else {
        var i = dlength
        while (i >= 0) {
          elementsZ(-dindex + zeroZ + strideZ * i) += alpha * elements(i) * elementsY(zeroY + strideY * i)
        }
      }
    } else {
      if (dindex >= 0) {
        var i = dlength
        while (i >= 0) {
          elementsZ(dindex + zeroZ + strideZ * i) += alpha * elements(i) * elementsY(zeroY + strideY * i)
        }
      } else {
        var i = dlength
        while (i >= 0) {
          elementsZ(zeroZ + strideZ * i) += alpha * elements(i) * elementsY(-dindex + zeroY + strideY * i)
        }
      }
    }
    z
  }

  //TODO: DenseColumn2D
  def multDenseColumn(A: DenseColumnDoubleMatrix2D, y: DenseColumnDoubleMatrix1D, z: DenseColumnDoubleMatrix1D, alpha: Double, beta: Double, transposeA: Boolean): DoubleMatrix1D = {
    if (z == null) {
      z = new DenseMatrix1D(if (transposeA) columns else rows)
    }
    if ((if (transposeA) rows else columns) != y.size || (if (transposeA) columns else rows) > z.size) throw new IllegalArgumentException("Incompatible args: " + toShapeString() + ", " + y.toShapeString() +
      ", " +
      z.toShapeString())
    if (! y.isInstanceOf[DenseMatrix1D] || ! z.isInstanceOf[DenseMatrix1D] ||
      this.isView ||
      y.isView ||
      z.isView) return super.zMult(y, z, alpha, beta, transposeA)
    val yElements = y.elements().asInstanceOf[Array[Double]]
    val zElements = z.elements().asInstanceOf[Array[Double]]
    val transA = if (transposeA) Transpose.Transpose else Transpose.NoTranspose
    BLAS.getInstance.dgemv(transA.netlib(), rows, columns, alpha, elements, Math.max(rows, 1), yElements,
      1, beta, zElements, 1)
    z
  }

  //TODO: DenseColumn2D
  def multDenseColumn(A: DoubleMatrix2D, B: DoubleMatrix2D, C: DoubleMatrix2D, alpha: Double, beta: Double, transposeA: Boolean, transposeB: Boolean): DoubleMatrix2D = {
    val rowsA = if (transposeA) columns else rows
    val columnsA = if (transposeA) rows else columns
    val rowsB = if (transposeB) B.columns else B.rows
    val columnsB = if (transposeB) B.rows else B.columns
    val rowsC = rowsA
    val columnsC = columnsB
    if (columnsA != rowsB) {
      throw new IllegalArgumentException("Matrix2D inner dimensions must agree:" + this.toShapeString() +
        ", " +
        B.toShapeString())
    }
    if (C == null) {
      C = new DenseColumnDoubleMatrix2D(rowsC, columnsC)
    } else {
      if (rowsA != C.rows || columnsB != C.columns) {
        throw new IllegalArgumentException("Incompatibe result matrix: " + this.toShapeString() +
          ", " +
          B.toShapeString() +
          ", " +
          C.toShapeString())
      }
    }
    if (this eq C || B eq C) throw new IllegalArgumentException("Matrices must not be identical")
    if (! B.isInstanceOf[DenseColumnDoubleMatrix2D] || !C.isInstanceOf[DenseColumnDoubleMatrix2D] ||
      this.isView ||
      B.isView ||
      C.isView) return super.zMult(B, C, alpha, beta, transposeA, transposeB)
    val transA = if (transposeA) Transpose.Transpose else Transpose.NoTranspose
    val transB = if (transposeB) Transpose.Transpose else Transpose.NoTranspose
    val elementsA = elements
    val elementsB = B.elements().asInstanceOf[Array[Double]]
    val elementsC = C.elements().asInstanceOf[Array[Double]]
    val lda = if (transposeA) Math.max(1, columnsA) else Math.max(1, rowsA)
    val ldb = if (transposeB) Math.max(1, columnsB) else Math.max(1, rowsB)
    val ldc = Math.max(1, rowsA)
    BLAS.getInstance.dgemm(transA.netlib(), transB.netlib(), rowsA, columnsB, columnsA, alpha, elementsA,
      lda, elementsB, ldb, beta, elementsC, ldc)
    C
  }
*/
}
