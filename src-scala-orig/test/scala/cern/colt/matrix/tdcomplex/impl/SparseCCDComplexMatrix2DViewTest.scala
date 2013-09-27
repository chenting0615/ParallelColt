package cern.colt.matrix.tdcomplex.impl

//remove if not needed
import scala.collection.JavaConversions._

class SparseCCDComplexMatrix2DViewTest(arg0: String) extends SparseCCDComplexMatrix2DTest(arg0) {

  protected def createMatrices() {
    A = new SparseCCDComplexMatrix2D(NCOLUMNS, NROWS).viewDice()
    B = new SparseCCDComplexMatrix2D(NCOLUMNS, NROWS).viewDice()
    Bt = new SparseCCDComplexMatrix2D(NROWS, NCOLUMNS).viewDice()
  }
}
