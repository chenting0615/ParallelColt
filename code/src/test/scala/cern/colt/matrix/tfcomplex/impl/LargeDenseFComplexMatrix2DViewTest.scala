package cern.colt.matrix.tfcomplex.impl

//remove if not needed
import scala.collection.JavaConversions._

class LargeDenseFComplexMatrix2DViewTest(arg0: String) extends LargeDenseFComplexMatrix2DTest(arg0) {

  protected def createMatrices() {
    A = new DenseLargeFComplexMatrix2D(NCOLUMNS, NROWS).viewDice()
    B = new DenseLargeFComplexMatrix2D(NCOLUMNS, NROWS).viewDice()
    Bt = new DenseLargeFComplexMatrix2D(NROWS, NCOLUMNS).viewDice()
  }
}
