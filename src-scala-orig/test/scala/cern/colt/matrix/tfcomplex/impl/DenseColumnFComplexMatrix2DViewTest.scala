package cern.colt.matrix.tfcomplex.impl

//remove if not needed
import scala.collection.JavaConversions._

class DenseColumnFComplexMatrix2DViewTest(arg0: String) extends DenseColumnFComplexMatrix2DTest(arg0) {

  protected def createMatrices() {
    A = new DenseColumnFComplexMatrix2D(NCOLUMNS, NROWS).viewDice()
    B = new DenseColumnFComplexMatrix2D(NCOLUMNS, NROWS).viewDice()
    Bt = new DenseColumnFComplexMatrix2D(NROWS, NCOLUMNS).viewDice()
  }
}
