package cern.colt.matrix.tdcomplex.impl

//remove if not needed
import scala.collection.JavaConversions._

class SparseCCMDComplexMatrix2DViewTest(arg0: String) extends SparseCCMDComplexMatrix2DTest(arg0) {

  protected def createMatrices() {
    A = new SparseCCMDComplexMatrix2D(NROWS, NCOLUMNS).viewDice()
    B = new SparseCCMDComplexMatrix2D(NROWS, NCOLUMNS).viewDice()
    Bt = new SparseCCMDComplexMatrix2D(NCOLUMNS, NROWS).viewDice()
  }
}
