package cern.colt.matrix.tdcomplex.impl

//remove if not needed
import scala.collection.JavaConversions._

class LargeDenseDComplexMatrix2DViewTest(arg0: String) extends LargeDenseDComplexMatrix2DTest(arg0) {

  protected def createMatrices() {
    A = new DenseLargeDComplexMatrix2D(NCOLUMNS, NROWS).viewDice()
    B = new DenseLargeDComplexMatrix2D(NCOLUMNS, NROWS).viewDice()
    Bt = new DenseLargeDComplexMatrix2D(NROWS, NCOLUMNS).viewDice()
  }
}
