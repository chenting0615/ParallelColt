package cern.colt.matrix.tdouble.impl

//remove if not needed
import scala.collection.JavaConversions._

class DiagonalDoubleMatrix2DViewTest(arg0: String) extends DiagonalDoubleMatrix2DTest(arg0) {

  protected def createMatrices() {
    DINDEX = 3
    A = new DiagonalDoubleMatrix2D(NCOLUMNS, NROWS, -DINDEX)
    DLENGTH = A.asInstanceOf[DiagonalDoubleMatrix2D].diagonalLength()
    A = A.viewDice()
    B = new DiagonalDoubleMatrix2D(NCOLUMNS, NROWS, -DINDEX)
      .viewDice()
    Bt = new DiagonalDoubleMatrix2D(NROWS, NCOLUMNS, DINDEX)
      .viewDice()
  }
}
