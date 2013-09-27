package cern.colt.matrix.tfcomplex.impl

//remove if not needed
import scala.collection.JavaConversions._

class DiagonalFComplexMatrix2DViewTest(arg0: String) extends DiagonalFComplexMatrix2DTest(arg0) {

  protected def createMatrices() {
    DINDEX = 3
    A = new DiagonalFComplexMatrix2D(NCOLUMNS, NROWS, -DINDEX)
    DLENGTH = A.asInstanceOf[DiagonalFComplexMatrix2D].diagonalLength()
    A = A.viewDice()
    B = new DiagonalFComplexMatrix2D(NCOLUMNS, NROWS, -DINDEX)
      .viewDice()
    Bt = new DiagonalFComplexMatrix2D(NROWS, NCOLUMNS, DINDEX)
      .viewDice()
  }
}
