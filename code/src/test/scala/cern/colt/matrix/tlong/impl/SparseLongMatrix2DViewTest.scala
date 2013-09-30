package cern.colt.matrix.tlong.impl

//remove if not needed
import scala.collection.JavaConversions._

class SparseLongMatrix2DViewTest(arg0: String) extends SparseLongMatrix2DTest(arg0) {

  protected def createMatrices() {
    A = new SparseLongMatrix2D(NCOLUMNS, NROWS).viewDice()
    B = new SparseLongMatrix2D(NCOLUMNS, NROWS).viewDice()
    Bt = new SparseLongMatrix2D(NROWS, NCOLUMNS).viewDice()
  }
}
