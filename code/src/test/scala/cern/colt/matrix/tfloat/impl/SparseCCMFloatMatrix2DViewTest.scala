package cern.colt.matrix.tfloat.impl

//remove if not needed
import scala.collection.JavaConversions._

class SparseCCMFloatMatrix2DViewTest(arg0: String) extends SparseCCMFloatMatrix2DTest(arg0) {

  protected def createMatrices() {
    A = new SparseCCMFloatMatrix2D(NCOLUMNS, NROWS).viewDice()
    B = new SparseCCMFloatMatrix2D(NCOLUMNS, NROWS).viewDice()
    Bt = new SparseCCMFloatMatrix2D(NROWS, NCOLUMNS).viewDice()
  }
}
