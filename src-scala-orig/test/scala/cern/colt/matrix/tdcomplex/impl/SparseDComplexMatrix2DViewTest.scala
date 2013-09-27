package cern.colt.matrix.tdcomplex.impl

//remove if not needed
import scala.collection.JavaConversions._

class SparseDComplexMatrix2DViewTest(arg0: String) extends SparseDComplexMatrix2DTest(arg0) {

  protected def createMatrices() {
    A = new SparseDComplexMatrix2D(NCOLUMNS, NROWS).viewDice()
    B = new SparseDComplexMatrix2D(NCOLUMNS, NROWS).viewDice()
    Bt = new SparseDComplexMatrix2D(NROWS, NCOLUMNS).viewDice()
  }
}
