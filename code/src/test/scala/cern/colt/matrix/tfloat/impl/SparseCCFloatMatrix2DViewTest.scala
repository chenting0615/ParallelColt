package cern.colt.matrix.tfloat.impl

//remove if not needed
import scala.collection.JavaConversions._

class SparseCCFloatMatrix2DViewTest(arg0: String) extends SparseCCFloatMatrix2DTest(arg0) {

  protected def createMatrices() {
    A = new SparseCCFloatMatrix2D(NCOLUMNS, NROWS).viewDice()
    B = new SparseCCFloatMatrix2D(NCOLUMNS, NROWS).viewDice()
    Bt = new SparseCCFloatMatrix2D(NROWS, NCOLUMNS).viewDice()
  }
}
