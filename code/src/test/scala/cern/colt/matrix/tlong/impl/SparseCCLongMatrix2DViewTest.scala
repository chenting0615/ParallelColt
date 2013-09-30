package cern.colt.matrix.tlong.impl

//remove if not needed
import scala.collection.JavaConversions._

class SparseCCLongMatrix2DViewTest(arg0: String) extends SparseCCLongMatrix2DTest(arg0) {

  protected def createMatrices() {
    A = new SparseCCLongMatrix2D(NCOLUMNS, NROWS).viewDice()
    B = new SparseCCLongMatrix2D(NCOLUMNS, NROWS).viewDice()
    Bt = new SparseCCLongMatrix2D(NROWS, NCOLUMNS).viewDice()
  }
}
