package cern.colt.matrix.tdouble.impl

//remove if not needed
import scala.collection.JavaConversions._

class SparseCCDoubleMatrix2DViewTest(arg0: String) extends SparseCCDoubleMatrix2DTest(arg0) {

  protected def createMatrices() {
    A = new SparseCCDoubleMatrix2D(NCOLUMNS, NROWS).viewDice()
    B = new SparseCCDoubleMatrix2D(NCOLUMNS, NROWS).viewDice()
    Bt = new SparseCCDoubleMatrix2D(NROWS, NCOLUMNS).viewDice()
  }
}
