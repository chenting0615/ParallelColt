package cern.colt.matrix.tdcomplex.impl

//remove if not needed
import scala.collection.JavaConversions._

class SparseRCMDComplexMatrix2DViewTest(arg0: String) extends SparseRCMDComplexMatrix2DTest(arg0) {

  protected def createMatrices() {
    A = new SparseRCMDComplexMatrix2D(NROWS, NCOLUMNS).viewDice()
    B = new SparseRCMDComplexMatrix2D(NROWS, NCOLUMNS).viewDice()
    Bt = new SparseRCMDComplexMatrix2D(NCOLUMNS, NROWS).viewDice()
  }
}
