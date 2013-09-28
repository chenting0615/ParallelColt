package cern.colt.matrix

import cern.colt.matrix.impl._

object MatrixClasses {
  trait DoubleMatrix1D extends Matrix1D[Double]
  class DenseColumnDoubleMatrix2D(rows: Int, columns: Int) extends DenseColumnMatrix2D[Double](rows, columns)
  class DenseDoubleMatrix1D(size: Int) extends DenseMatrix1D[Double](size)
  class DenseDoubleMatrix2D(rows: Int, columns: Int) extends DenseMatrix2D[Double](rows, columns)
  class DenseLargeDoubleMatrix2D(rows: Int, columns: Int) extends DenseLargeMatrix2D[Double](rows, columns)
  class DiagonalDoubleMatrix2D(rows: Int, columns: Int) extends DiagonalMatrix2D[Double](rows, columns)
  class WrapperDoubleMatrix1D(content: Matrix1D[Double]) extends WrapperMatrix1D[Double](content)
  class WrapperDoubleMatrix2D(content: Matrix2D[Double]) extends WrapperMatrix2D[Double](content)
  class SparseCCDoubleMatrix2D(rows: Int, columns: Int) extends SparseCCMatrix2D[Double](rows, columns)
  class SparseCCMDoubleMatrix2D(rows: Int, columns: Int) extends SparseCCMMatrix2D[Double](rows, columns)
  class SparseDoubleMatrix1D(size: Int) extends SparseMatrix1D[Double](size)
  class SparseDoubleMatrix2D(rows: Int, columns: Int) extends SparseHashMatrix2D[Double](rows, columns)
  class SparseRCDoubleMatrix2D(rows: Int, columns: Int) extends SparseRCMatrix2D[Double](rows, columns)
  class SparseRCMDoubleMatrix2D(rows: Int, columns: Int) extends SparseRCMMatrix2D[Double](rows, columns)

  class DenseColumnFloatMatrix2D(rows: Int, columns: Int) extends DenseColumnMatrix2D[Float](rows, columns)
  class DenseFloatMatrix1D(size: Int) extends DenseMatrix1D[Float](size)
  class DenseFloatMatrix2D(rows: Int, columns: Int) extends DenseMatrix2D[Float](rows, columns)
  class DenseLargeFloatMatrix2D(rows: Int, columns: Int) extends DenseLargeMatrix2D[Float](rows, columns)
  class DiagonalFloatMatrix2D(rows: Int, columns: Int) extends DiagonalMatrix2D[Float](rows, columns)
  class WrapperFloatMatrix1D(content: Matrix1D[Float]) extends WrapperMatrix1D[Float](content)
  class WrapperFloatMatrix2D(content: Matrix2D[Float]) extends WrapperMatrix2D[Float](content)
  //class SparseCCFloatMatrix2D(rows: Int, columns: Int) extends SparseCCMatrix2D[Float](rows, columns)
  class SparseCCMFloatMatrix2D(rows: Int, columns: Int) extends SparseCCMMatrix2D[Float](rows, columns)
  class SparseFloatMatrix1D(size: Int) extends SparseMatrix1D[Float](size)
  class SparseFloatMatrix2D(rows: Int, columns: Int) extends SparseHashMatrix2D[Float](rows, columns)
  //class SparseRCFloatMatrix2D(rows: Int, columns: Int) extends SparseRCMatrix2D[Float](rows, columns)
  class SparseRCMFloatMatrix2D(rows: Int, columns: Int) extends SparseRCMMatrix2D[Float](rows, columns)

  class DenseColumnIntMatrix2D(rows: Int, columns: Int) extends DenseColumnMatrix2D[Int](rows, columns)
  class DenseIntMatrix1D(size: Int) extends DenseMatrix1D[Int](size)
  class DenseIntMatrix2D(rows: Int, columns: Int) extends DenseMatrix2D[Int](rows, columns)
  class DenseLargeIntMatrix2D(rows: Int, columns: Int) extends DenseLargeMatrix2D[Int](rows, columns)
  class DiagonalIntMatrix2D(rows: Int, columns: Int) extends DiagonalMatrix2D[Int](rows, columns)
  class WrapperIntMatrix1D(content: Matrix1D[Int]) extends WrapperMatrix1D[Int](content)
  class WrapperIntMatrix2D(content: Matrix2D[Int]) extends WrapperMatrix2D[Int](content)
  //class SparseCCIntMatrix2D(rows: Int, columns: Int) extends SparseCCMatrix2D[Int](rows, columns)
  class SparseCCMIntMatrix2D(rows: Int, columns: Int) extends SparseCCMMatrix2D[Int](rows, columns)
  class SparseIntMatrix1D(size: Int) extends SparseMatrix1D[Int](size)
  class SparseIntMatrix2D(rows: Int, columns: Int) extends SparseHashMatrix2D[Int](rows, columns)
  //class SparseRCIntMatrix2D(rows: Int, columns: Int) extends SparseRCMatrix2D[Int](rows, columns)
  class SparseRCMIntMatrix2D(rows: Int, columns: Int) extends SparseRCMMatrix2D[Int](rows, columns)

  class DenseColumnLongMatrix2D(rows: Int, columns: Int) extends DenseColumnMatrix2D[Long](rows, columns)
  class DenseLargeLongMatrix2D(rows: Int, columns: Int) extends DenseLargeMatrix2D[Long](rows, columns)
  class DenseLongMatrix1D(size: Int) extends DenseMatrix1D[Long](size)
  class DenseLongMatrix2D(rows: Int, columns: Int) extends DenseMatrix2D[Long](rows, columns)
  class DiagonalLongMatrix2D(rows: Int, columns: Int) extends DiagonalMatrix2D[Long](rows, columns)
  class WrapperLongMatrix1D(content: Matrix1D[Long]) extends WrapperMatrix1D[Long](content)
  class WrapperLongMatrix2D(content: Matrix2D[Long]) extends WrapperMatrix2D[Long](content)
  //class SparseCCLongMatrix2D(rows: Int, columns: Int) extends SparseCCMatrix2D[Long](rows, columns)
  class SparseCCMLongMatrix2D(rows: Int, columns: Int) extends SparseCCMMatrix2D[Long](rows, columns)
  class SparseLongMatrix1D(size: Int) extends SparseMatrix1D[Long](size)
  class SparseLongMatrix2D(rows: Int, columns: Int) extends SparseHashMatrix2D[Long](rows, columns)
  //class SparseRCLongMatrix2D(rows: Int, columns: Int) extends SparseRCMatrix2D[Long](rows, columns)
  class SparseRCMLongMatrix2D(rows: Int, columns: Int) extends SparseRCMMatrix2D[Long](rows, columns)

  class DenseColumnObjectMatrix2D(rows: Int, columns: Int) extends DenseColumnMatrix2D[Object](rows, columns)
  class DenseLargeObjectMatrix2D(rows: Int, columns: Int) extends DenseLargeMatrix2D[Object](rows, columns)
  class DenseObjectMatrix1D(size: Int) extends DenseMatrix1D[Object](size)
  class DenseObjectMatrix2D(rows: Int, columns: Int) extends DenseMatrix2D[Object](rows, columns)
  //class DiagonalObjectMatrix2D(rows: Int, columns: Int) extends DiagonalMatrix2D[Object](rows, columns)
  class WrapperObjectMatrix1D(content: Matrix1D[Object]) extends WrapperMatrix1D[Object](content)
  class WrapperObjectMatrix2D(content: Matrix2D[Object]) extends WrapperMatrix2D[Object](content)
  //class SparseCCObjectMatrix2D(rows: Int, columns: Int) extends SparseCCMatrix2D[Object](rows, columns)
  // class SparseCCMObjectMatrix2D(rows: Int, columns: Int) extends SparseCCMMatrix2D[Object](rows, columns)
  // class SparseObjectMatrix1D(size: Int) extends SparseMatrix1D[Object](size)
  //class SparseObjectMatrix2D(rows: Int, columns: Int) extends SparseHashMatrix2D[Object](rows, columns)
  //class SparseRCObjectMatrix2D(rows: Int, columns: Int) extends SparseRCMatrix2D[Object](rows, columns)
  //class SparseRCMObjectMatrix2D(rows: Int, columns: Int) extends SparseRCMMatrix2D[Object](rows, columns)
}
