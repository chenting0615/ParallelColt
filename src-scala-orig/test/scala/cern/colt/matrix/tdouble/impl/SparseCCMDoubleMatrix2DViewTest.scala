package cern.colt.matrix.tdouble.impl

//remove if not needed
import scala.collection.JavaConversions._

class SparseCCMDoubleMatrix2DViewTest(arg0: String) extends SparseCCMDoubleMatrix2DTest(arg0) {

  protected def createMatrices() {
    A = new SparseCCMDoubleMatrix2D(NCOLUMNS, NROWS).viewDice()
    B = new SparseCCMDoubleMatrix2D(NCOLUMNS, NROWS).viewDice()
    Bt = new SparseCCMDoubleMatrix2D(NROWS, NCOLUMNS).viewDice()
  }
}
