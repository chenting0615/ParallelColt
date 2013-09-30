package cern.colt.matrix.tfloat.impl

//remove if not needed
import scala.collection.JavaConversions._

class DiagonalFloatMatrix2DViewTest(arg0: String) extends DiagonalFloatMatrix2DTest(arg0) {

  protected def createMatrices() {
    DINDEX = 3
    A = new DiagonalFloatMatrix2D(NCOLUMNS, NROWS, -DINDEX)
    DLENGTH = A.asInstanceOf[DiagonalFloatMatrix2D].diagonalLength()
    A = A.viewDice()
    B = new DiagonalFloatMatrix2D(NCOLUMNS, NROWS, -DINDEX)
      .viewDice()
    Bt = new DiagonalFloatMatrix2D(NROWS, NCOLUMNS, DINDEX).viewDice()
  }
}
