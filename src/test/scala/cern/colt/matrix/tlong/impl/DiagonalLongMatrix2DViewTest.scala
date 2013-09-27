package cern.colt.matrix.tlong.impl

//remove if not needed
import scala.collection.JavaConversions._

class DiagonalLongMatrix2DViewTest(arg0: String) extends DiagonalLongMatrix2DTest(arg0) {

  protected def createMatrices() {
    DINDEX = 3
    A = new DiagonalLongMatrix2D(NCOLUMNS, NROWS, -DINDEX)
    DLENGTH = A.asInstanceOf[DiagonalLongMatrix2D].diagonalLength()
    A = A.viewDice()
    B = new DiagonalLongMatrix2D(NCOLUMNS, NROWS, -DINDEX).viewDice()
    Bt = new DiagonalLongMatrix2D(NROWS, NCOLUMNS, DINDEX).viewDice()
  }
}
