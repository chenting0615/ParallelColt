package cern.colt.matrix.tdouble.impl

import cern.colt.matrix.MatrixTypes.DiagonalDoubleMatrix2D

class DiagonalDoubleMatrix2DViewTest(arg0: String) extends DiagonalDoubleMatrix2DTest(arg0) {

  override protected def createMatrices() {
    DINDEX = 3
    A = new DiagonalDoubleMatrix2D(NCOLUMNS, NROWS, -DINDEX)
    DLENGTH = A.asInstanceOf[DiagonalDoubleMatrix2D].diagonalLength
    A = A.viewTranspose()
    B = new DiagonalDoubleMatrix2D(NCOLUMNS, NROWS, -DINDEX).viewTranspose()
    Bt = new DiagonalDoubleMatrix2D(NROWS, NCOLUMNS, DINDEX).viewTranspose()
  }
}
