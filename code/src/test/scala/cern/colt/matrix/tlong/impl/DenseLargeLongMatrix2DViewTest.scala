package cern.colt.matrix.tlong.impl

//remove if not needed
import scala.collection.JavaConversions._

class DenseLargeLongMatrix2DViewTest(arg0: String) extends DenseLargeLongMatrix2DTest(arg0) {

  protected def createMatrices() {
    A = new DenseLargeLongMatrix2D(NCOLUMNS, NROWS).viewDice()
    B = new DenseLargeLongMatrix2D(NCOLUMNS, NROWS).viewDice()
    Bt = new DenseLargeLongMatrix2D(NROWS, NCOLUMNS).viewDice()
  }
}
