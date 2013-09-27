package cern.colt.matrix.tdouble.impl

//remove if not needed
import scala.collection.JavaConversions._

class DenseLargeDoubleMatrix2DViewTest(arg0: String) extends DenseLargeDoubleMatrix2DTest(arg0) {

  protected def createMatrices() {
    A = new DenseLargeDoubleMatrix2D(NCOLUMNS, NROWS).viewDice()
    B = new DenseLargeDoubleMatrix2D(NCOLUMNS, NROWS).viewDice()
    Bt = new DenseLargeDoubleMatrix2D(NROWS, NCOLUMNS).viewDice()
  }
}
