package cern.colt.matrix.tfcomplex.impl

//remove if not needed
import scala.collection.JavaConversions._

class DenseFComplexMatrix2DViewTest(arg0: String) extends DenseFComplexMatrix2DTest(arg0) {

  protected def createMatrices() {
    A = new DenseFComplexMatrix2D(NCOLUMNS, NROWS).viewDice()
    B = new DenseFComplexMatrix2D(NCOLUMNS, NROWS).viewDice()
    Bt = new DenseFComplexMatrix2D(NROWS, NCOLUMNS).viewDice()
  }
}
