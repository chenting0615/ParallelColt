package cern.colt.matrix.tfloat.impl

//remove if not needed
import scala.collection.JavaConversions._

class DenseFloatMatrix3DViewTest(arg0: String) extends DenseFloatMatrix3DTest(arg0) {

  protected def createMatrices() {
    A = new DenseFloatMatrix3D(NCOLUMNS, NROWS, NSLICES).viewDice(2, 1, 0)
    B = new DenseFloatMatrix3D(NCOLUMNS, NROWS, NSLICES).viewDice(2, 1, 0)
  }
}
