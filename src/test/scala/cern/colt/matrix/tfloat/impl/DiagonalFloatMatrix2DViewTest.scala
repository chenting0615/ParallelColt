package cern.colt.matrix.tfloat.impl

import cern.colt.matrix.MatrixTypes.DiagonalFloatMatrix2D

class DiagonalFloatMatrix2DViewTest(arg0: String) extends DiagonalFloatMatrix2DTest(arg0) {

  override protected def createMatrices() {
    DINDEX = 3
    A = new DiagonalFloatMatrix2D(NCOLUMNS, NROWS, -DINDEX)
    DLENGTH = A.asInstanceOf[DiagonalFloatMatrix2D].diagonalLength
    A = A.viewTranspose()
    B = new DiagonalFloatMatrix2D(NCOLUMNS, NROWS, -DINDEX).viewTranspose()
    Bt = new DiagonalFloatMatrix2D(NROWS, NCOLUMNS, DINDEX).viewTranspose()
  }
}
