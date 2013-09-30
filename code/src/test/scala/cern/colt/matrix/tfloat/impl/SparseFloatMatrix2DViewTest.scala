package cern.colt.matrix.tfloat.impl

//remove if not needed
import scala.collection.JavaConversions._

class SparseFloatMatrix2DViewTest(arg0: String) extends SparseFloatMatrix2DTest(arg0) {

  protected def createMatrices() {
    A = new SparseFloatMatrix2D(NCOLUMNS, NROWS).viewDice()
    B = new SparseFloatMatrix2D(NCOLUMNS, NROWS).viewDice()
    Bt = new SparseFloatMatrix2D(NROWS, NCOLUMNS).viewDice()
  }
}
