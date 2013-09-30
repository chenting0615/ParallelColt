package cern.colt.matrix.tdouble.impl

//remove if not needed
import scala.collection.JavaConversions._

class DenseLargeDoubleMatrix2DViewTest(arg0: String) extends DenseLargeMatrix2DTest(arg0) {

  protected def createMatrices() {
    A = new DenseLargeMatrix2D(NCOLUMNS, NROWS).viewDice()
    B = new DenseLargeMatrix2D(NCOLUMNS, NROWS).viewDice()
    Bt = new DenseLargeMatrix2D(NROWS, NCOLUMNS).viewDice()
  }
}
