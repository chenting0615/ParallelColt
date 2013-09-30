package cern.colt.matrix.tint.impl

//remove if not needed
import scala.collection.JavaConversions._

class SparseCCMIntMatrix2DViewTest(arg0: String) extends SparseCCMIntMatrix2DTest(arg0) {

  protected def createMatrices() {
    A = new SparseCCMIntMatrix2D(NCOLUMNS, NROWS).viewDice()
    B = new SparseCCMIntMatrix2D(NCOLUMNS, NROWS).viewDice()
    Bt = new SparseCCMIntMatrix2D(NROWS, NCOLUMNS).viewDice()
  }
}
