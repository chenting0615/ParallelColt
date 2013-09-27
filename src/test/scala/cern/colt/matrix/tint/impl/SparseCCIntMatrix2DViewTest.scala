package cern.colt.matrix.tint.impl

//remove if not needed
import scala.collection.JavaConversions._

class SparseCCIntMatrix2DViewTest(arg0: String) extends SparseCCIntMatrix2DTest(arg0) {

  protected def createMatrices() {
    A = new SparseCCIntMatrix2D(NCOLUMNS, NROWS).viewDice()
    B = new SparseCCIntMatrix2D(NCOLUMNS, NROWS).viewDice()
    Bt = new SparseCCIntMatrix2D(NROWS, NCOLUMNS).viewDice()
  }
}
