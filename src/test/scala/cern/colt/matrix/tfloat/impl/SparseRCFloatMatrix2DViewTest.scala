package cern.colt.matrix.tfloat.impl

//remove if not needed
import scala.collection.JavaConversions._

class SparseRCFloatMatrix2DViewTest(arg0: String) extends SparseRCFloatMatrix2DTest(arg0) {

  protected def createMatrices() {
    A = new SparseRCFloatMatrix2D(NCOLUMNS, NROWS).viewDice()
    B = new SparseRCFloatMatrix2D(NCOLUMNS, NROWS).viewDice()
    Bt = new SparseRCFloatMatrix2D(NROWS, NCOLUMNS).viewDice()
  }
}
