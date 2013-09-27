package cern.colt.matrix.tfloat.impl

//remove if not needed
import scala.collection.JavaConversions._

class DenseFloatMatrix2DViewTest(arg0: String) extends DenseFloatMatrix2DTest(arg0) {

  protected def createMatrices() {
    A = new DenseFloatMatrix2D(NCOLUMNS, NROWS).viewDice()
    B = new DenseFloatMatrix2D(NCOLUMNS, NROWS).viewDice()
    Bt = new DenseFloatMatrix2D(NROWS, NCOLUMNS).viewDice()
  }
}
