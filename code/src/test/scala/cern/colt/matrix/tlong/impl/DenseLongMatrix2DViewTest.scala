package cern.colt.matrix.tlong.impl

//remove if not needed
import scala.collection.JavaConversions._

class DenseLongMatrix2DViewTest(arg0: String) extends DenseLongMatrix2DTest(arg0) {

  protected def createMatrices() {
    A = new DenseLongMatrix2D(NCOLUMNS, NROWS).viewDice()
    B = new DenseLongMatrix2D(NCOLUMNS, NROWS).viewDice()
    Bt = new DenseLongMatrix2D(NROWS, NCOLUMNS).viewDice()
  }
}
