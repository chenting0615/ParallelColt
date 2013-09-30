package cern.colt.matrix.tfcomplex.impl

//remove if not needed
import scala.collection.JavaConversions._

class SparseCCFComplexMatrix2DViewTest(arg0: String) extends SparseCCFComplexMatrix2DTest(arg0) {

  protected def createMatrices() {
    A = new SparseCCFComplexMatrix2D(NCOLUMNS, NROWS).viewDice()
    B = new SparseCCFComplexMatrix2D(NCOLUMNS, NROWS).viewDice()
    Bt = new SparseCCFComplexMatrix2D(NROWS, NCOLUMNS).viewDice()
  }
}
