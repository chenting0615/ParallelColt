package cern.colt.matrix.tlong.impl

//remove if not needed
import scala.collection.JavaConversions._

class DenseColumnLongMatrix2DViewTest(arg0: String) extends DenseColumnLongMatrix2DTest(arg0) {

  protected def createMatrices() {
    A = new DenseColumnLongMatrix2D(NCOLUMNS, NROWS).viewDice()
    B = new DenseColumnLongMatrix2D(NCOLUMNS, NROWS).viewDice()
    Bt = new DenseColumnLongMatrix2D(NROWS, NCOLUMNS).viewDice()
  }
}
