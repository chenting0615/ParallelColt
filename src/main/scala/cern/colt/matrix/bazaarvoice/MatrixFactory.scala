package cern.colt.matrix.bazaarvoice

import java.util.HashMap
import java.util.Map
import cern.colt.matrix.MatrixTypes._
import cern.colt.matrix.impl.CompositeVerticalMatrix2D

object MatrixFactory {

/*
  abstract class DenseDouble2DBase extends DoubleFactory2D {

    override def make(values: Array[Array[Double]]): DoubleMatrix2D = {
      val result = make(values.length, values(0).length)
      result.assign(values)
      result
    }

    override def make1D(size: Int): DoubleMatrix1D = new DenseDoubleMatrix1D(size)
  }

  abstract class SparseDouble2DBase extends DenseDouble2DBase {

    override def make1D(size: Int): DoubleMatrix1D = new SparseDoubleMatrix1D(size)
  }

  class LikeMatrixDouble2D(patternMatrix: DoubleMatrix2D) extends DenseDouble2DBase {

    private var patternMatrix2D: DoubleMatrix2D = patternMatrix

    private var patternMatrix1D: DoubleMatrix1D = _

    def this(patternMatrix: DoubleMatrix1D) {
      this()
      this.patternMatrix1D = patternMatrix
    }

    override def make(rows: Int, columns: Int): DoubleMatrix2D = {
      if (patternMatrix2D != null) patternMatrix2D.like(rows, columns) else patternMatrix1D.like2D(rows,
        columns)
    }

    override def make1D(size: Int): DoubleMatrix1D = {
      if (patternMatrix2D != null) patternMatrix2D.like1D(size) else patternMatrix1D.like(size)
    }
  }

  class DenseColumnDouble2D extends DenseDouble2DBase {

    override def make(rows: Int, columns: Int): DoubleMatrix2D = {
      new DenseColumnDoubleMatrix2D(rows, columns)
    }

    override def make(values: Array[Array[Double]]): DoubleMatrix2D = new DenseColumnDoubleMatrix2D(values)
  }

  var DENSE_COLUMN_DOUBLE2D: DoubleFactory2D = new DenseColumnDouble2D()

  class DenseSizeDependentDouble2D(val maxSizeBeforeLarge: Int) extends DenseDouble2DBase {

    def this() {
      this(Integer.MAX_VALUE)
    }

    override def make(rows: Int, columns: Int): DoubleMatrix2D = {
      if (rows.toLong * columns >= maxSizeBeforeLarge.toLong) new DenseLargeDoubleMatrix2D(rows, columns) else new DenseDoubleMatrix2D(rows,
        columns)
    }

    override def make(values: Array[Array[Double]]): DoubleMatrix2D = {
      val rows = values.length
      val columns = values(0).length
      if (rows.toLong * columns >= maxSizeBeforeLarge.toLong) super.make(values) else new DenseDoubleMatrix2D(values)
    }
  }

  var DENSE_ROW_DOUBLE2D: DoubleFactory2D = new DenseSizeDependentDouble2D()

  private class DenseLargeDouble2D extends DenseDouble2DBase {

    override def make(rows: Int, columns: Int): DoubleMatrix2D = {
      new DenseLargeDoubleMatrix2D(rows, columns)
    }
  }

  var DENSE_LARGE_DOUBLE2D: DoubleFactory2D = new DenseLargeDouble2D()

  class SparseCCDouble2D extends SparseDouble2DBase {

    override def make(rows: Int, columns: Int): DoubleMatrix2D = {
      new SparseCCDoubleMatrix2D(rows, columns)
    }

    override def make(values: Array[Array[Double]]): DoubleMatrix2D = new SparseCCDoubleMatrix2D(values)
  }

  var SPARSE_CC_DOUBLE2D: DoubleFactory2D = new SparseCCDouble2D()

  class SparseCCMDouble2D extends SparseDouble2DBase {

    override def make(rows: Int, columns: Int): DoubleMatrix2D = {
      new SparseCCMDoubleMatrix2D(rows, columns)
    }
  }

  var SPARSE_CCM_DOUBLE2D: DoubleFactory2D = new SparseCCMDouble2D()

  class SparseRCDouble2D extends SparseDouble2DBase {

    override def make(rows: Int, columns: Int): DoubleMatrix2D = {
      new SparseRCDoubleMatrix2D(rows, columns)
    }

    override def make(values: Array[Array[Double]]): DoubleMatrix2D = new SparseRCDoubleMatrix2D(values)
  }

  var SPARSE_RC_DOUBLE2D: DoubleFactory2D = new SparseRCDouble2D()

  class SparseRCMDouble2D extends SparseDouble2DBase {

    override def make(rows: Int, columns: Int): DoubleMatrix2D = {
      new SparseRCMDoubleMatrix2D(rows, columns)
    }
  }

  var SPARSE_RCM_DOUBLE2D: DoubleFactory2D = new SparseRCMDouble2D()

  class SparseHashMDouble2D extends SparseDouble2DBase {

    override def make(rows: Int, columns: Int): DoubleMatrix2D = new SparseDoubleMatrix2D(rows, columns)
  }

  var SPARSE_HASH_DOUBLE2D: DoubleFactory2D = new SparseHashMDouble2D()

  class CompositDouble2D extends DenseDouble2DBase {

    override def make(rows: Int, columns: Int): DoubleMatrix2D = {
      new CompositeVerticalMatrix2D(rows, columns)
    }
  }

  var COMPOSITE_DOUBLE2D: DoubleFactory2D = new CompositDouble2D()

  private var factoryDouble2DMap: Map[String, DoubleFactory2D] = new HashMap[String, DoubleFactory2D]()

  factoryDouble2DMap.put("dense", DENSE_ROW_DOUBLE2D)

  factoryDouble2DMap.put("dense_row", DENSE_ROW_DOUBLE2D)

  factoryDouble2DMap.put("dense_column", DENSE_COLUMN_DOUBLE2D)

  factoryDouble2DMap.put("dense_large", DENSE_LARGE_DOUBLE2D)

  factoryDouble2DMap.put("sparse", SPARSE_HASH_DOUBLE2D)

  factoryDouble2DMap.put("sparse_hash", SPARSE_HASH_DOUBLE2D)

  factoryDouble2DMap.put("sparse_column", SPARSE_CC_DOUBLE2D)

  factoryDouble2DMap.put("sparse_cc", SPARSE_CC_DOUBLE2D)

  factoryDouble2DMap.put("sparse_ccm", SPARSE_CCM_DOUBLE2D)

  factoryDouble2DMap.put("sparse_row", SPARSE_RC_DOUBLE2D)

  factoryDouble2DMap.put("sparse_rc", SPARSE_RC_DOUBLE2D)

  factoryDouble2DMap.put("sparse_rcm", SPARSE_RCM_DOUBLE2D)

  factoryDouble2DMap.put("composite", COMPOSITE_DOUBLE2D)

  def getDouble2D(factoryName: String): DoubleFactory2D = {
    factoryDouble2DMap.get(factoryName.toLowerCase)
  }
*/
}
