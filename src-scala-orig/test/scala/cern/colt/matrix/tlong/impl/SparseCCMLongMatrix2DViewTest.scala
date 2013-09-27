package cern.colt.matrix.tlong.impl

//remove if not needed
import scala.collection.JavaConversions._

class SparseCCMLongMatrix2DViewTest(arg0: String) extends SparseCCMLongMatrix2DTest(arg0) {

  protected def createMatrices() {
    A = new SparseCCMLongMatrix2D(NCOLUMNS, NROWS).viewDice()
    B = new SparseCCMLongMatrix2D(NCOLUMNS, NROWS).viewDice()
    Bt = new SparseCCMLongMatrix2D(NROWS, NCOLUMNS).viewDice()
  }
}
