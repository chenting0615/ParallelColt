package cern.colt.matrix.tint.impl

//remove if not needed
import scala.collection.JavaConversions._

class DiagonalIntMatrix2DViewTest(arg0: String) extends DiagonalIntMatrix2DTest(arg0) {

  protected def createMatrices() {
    DINDEX = 3
    A = new DiagonalIntMatrix2D(NCOLUMNS, NROWS, -DINDEX)
    DLENGTH = A.asInstanceOf[DiagonalIntMatrix2D].diagonalLength()
    A = A.viewDice()
    B = new DiagonalIntMatrix2D(NCOLUMNS, NROWS, -DINDEX).viewDice()
    Bt = new DiagonalIntMatrix2D(NROWS, NCOLUMNS, DINDEX).viewDice()
  }
}
