package cern.colt.matrix.tdcomplex.impl

//remove if not needed
import scala.collection.JavaConversions._

class DenseDComplexMatrix2DViewTest(arg0: String) extends DenseDComplexMatrix2DTest(arg0) {

  protected def createMatrices() {
    A = new DenseDComplexMatrix2D(NCOLUMNS, NROWS).viewDice()
    B = new DenseDComplexMatrix2D(NCOLUMNS, NROWS).viewDice()
    Bt = new DenseDComplexMatrix2D(NROWS, NCOLUMNS).viewDice()
  }
}
