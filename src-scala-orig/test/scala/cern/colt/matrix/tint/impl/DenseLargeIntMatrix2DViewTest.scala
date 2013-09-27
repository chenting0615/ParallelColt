package cern.colt.matrix.tint.impl

//remove if not needed
import scala.collection.JavaConversions._

class DenseLargeIntMatrix2DViewTest(arg0: String) extends DenseLargeIntMatrix2DTest(arg0) {

  protected def createMatrices() {
    A = new DenseLargeIntMatrix2D(NCOLUMNS, NROWS).viewDice()
    B = new DenseLargeIntMatrix2D(NCOLUMNS, NROWS).viewDice()
    Bt = new DenseLargeIntMatrix2D(NROWS, NCOLUMNS).viewDice()
  }
}
