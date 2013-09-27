package cern.colt.matrix.tdcomplex.impl

//remove if not needed
import scala.collection.JavaConversions._

class DiagonalDComplexMatrix2DViewTest(arg0: String) extends DiagonalDComplexMatrix2DTest(arg0) {

  protected def createMatrices() {
    DINDEX = 3
    A = new DiagonalDComplexMatrix2D(NCOLUMNS, NROWS, -DINDEX)
    DLENGTH = A.asInstanceOf[DiagonalDComplexMatrix2D].diagonalLength()
    A = A.viewDice()
    B = new DiagonalDComplexMatrix2D(NCOLUMNS, NROWS, -DINDEX)
      .viewDice()
    Bt = new DiagonalDComplexMatrix2D(NROWS, NCOLUMNS, DINDEX)
      .viewDice()
  }
}
