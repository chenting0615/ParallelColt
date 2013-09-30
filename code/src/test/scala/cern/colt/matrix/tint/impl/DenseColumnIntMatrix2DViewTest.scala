package cern.colt.matrix.tint.impl

//remove if not needed
import scala.collection.JavaConversions._

class DenseColumnIntMatrix2DViewTest(arg0: String) extends DenseColumnIntMatrix2DTest(arg0) {

  protected def createMatrices() {
    A = new DenseColumnIntMatrix2D(NCOLUMNS, NROWS).viewDice()
    B = new DenseColumnIntMatrix2D(NCOLUMNS, NROWS).viewDice()
    Bt = new DenseColumnIntMatrix2D(NROWS, NCOLUMNS).viewDice()
  }
}
