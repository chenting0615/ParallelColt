package cern.colt.matrix.tint.impl

//remove if not needed
import scala.collection.JavaConversions._

class DenseIntMatrix2DViewTest(arg0: String) extends DenseIntMatrix2DTest(arg0) {

  protected def createMatrices() {
    A = new DenseIntMatrix2D(NCOLUMNS, NROWS).viewDice()
    B = new DenseIntMatrix2D(NCOLUMNS, NROWS).viewDice()
    Bt = new DenseIntMatrix2D(NROWS, NCOLUMNS).viewDice()
  }
}
