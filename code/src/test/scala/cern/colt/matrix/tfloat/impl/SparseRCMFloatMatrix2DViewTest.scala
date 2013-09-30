package cern.colt.matrix.tfloat.impl

//remove if not needed
import scala.collection.JavaConversions._

class SparseRCMFloatMatrix2DViewTest(arg0: String) extends SparseRCMFloatMatrix2DTest(arg0) {

  protected def createMatrices() {
    A = new SparseRCMFloatMatrix2D(NCOLUMNS, NROWS).viewDice()
    B = new SparseRCMFloatMatrix2D(NCOLUMNS, NROWS).viewDice()
    Bt = new SparseRCMFloatMatrix2D(NROWS, NCOLUMNS).viewDice()
  }
}
