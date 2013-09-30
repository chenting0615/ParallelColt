package cern.colt.matrix.tfloat.impl

//remove if not needed
import scala.collection.JavaConversions._

class DenseLargeFloatMatrix2DViewTest(arg0: String) extends DenseLargeFloatMatrix2DTest(arg0) {

  protected def createMatrices() {
    A = new DenseLargeFloatMatrix2D(NCOLUMNS, NROWS).viewDice()
    B = new DenseLargeFloatMatrix2D(NCOLUMNS, NROWS).viewDice()
    Bt = new DenseLargeFloatMatrix2D(NROWS, NCOLUMNS).viewDice()
  }
}
