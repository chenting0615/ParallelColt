package cern.colt.matrix.tdcomplex.impl

//remove if not needed
import scala.collection.JavaConversions._

class DenseColumnDComplexMatrix2DViewTest(arg0: String) extends DenseColumnDComplexMatrix2DTest(arg0) {

  protected def createMatrices() {
    A = new DenseColumnDComplexMatrix2D(NCOLUMNS, NROWS).viewDice()
    B = new DenseColumnDComplexMatrix2D(NCOLUMNS, NROWS).viewDice()
    Bt = new DenseColumnDComplexMatrix2D(NROWS, NCOLUMNS).viewDice()
  }
}
