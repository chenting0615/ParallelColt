package cern.colt.matrix.tfloat.impl

//remove if not needed
import scala.collection.JavaConversions._

class DenseColumnFloatMatrix2DViewTest(arg0: String) extends DenseColumnFloatMatrix2DTest(arg0) {

  protected def createMatrices() {
    A = new DenseColumnFloatMatrix2D(NCOLUMNS, NROWS).viewDice()
    B = new DenseColumnFloatMatrix2D(NCOLUMNS, NROWS).viewDice()
    Bt = new DenseColumnFloatMatrix2D(NROWS, NCOLUMNS).viewDice()
  }
}
