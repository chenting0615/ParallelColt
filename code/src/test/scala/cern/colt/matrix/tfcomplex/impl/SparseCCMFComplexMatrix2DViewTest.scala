package cern.colt.matrix.tfcomplex.impl

//remove if not needed
import scala.collection.JavaConversions._

class SparseCCMFComplexMatrix2DViewTest(arg0: String) extends SparseCCMFComplexMatrix2DTest(arg0) {

  protected def createMatrices() {
    A = new SparseCCMFComplexMatrix2D(NROWS, NCOLUMNS).viewDice()
    B = new SparseCCMFComplexMatrix2D(NROWS, NCOLUMNS).viewDice()
    Bt = new SparseCCMFComplexMatrix2D(NCOLUMNS, NROWS).viewDice()
  }
}
