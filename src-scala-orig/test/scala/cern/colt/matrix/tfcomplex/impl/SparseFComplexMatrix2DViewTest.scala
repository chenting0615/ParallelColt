package cern.colt.matrix.tfcomplex.impl

//remove if not needed
import scala.collection.JavaConversions._

class SparseFComplexMatrix2DViewTest(arg0: String) extends SparseFComplexMatrix2DTest(arg0) {

  protected def createMatrices() {
    A = new SparseFComplexMatrix2D(NCOLUMNS, NROWS).viewDice()
    B = new SparseFComplexMatrix2D(NCOLUMNS, NROWS).viewDice()
    Bt = new SparseFComplexMatrix2D(NROWS, NCOLUMNS).viewDice()
  }
}
