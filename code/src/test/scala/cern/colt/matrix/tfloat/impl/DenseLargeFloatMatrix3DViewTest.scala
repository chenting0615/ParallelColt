package cern.colt.matrix.tfloat.impl

//remove if not needed
import scala.collection.JavaConversions._

class DenseLargeFloatMatrix3DViewTest(arg0: String) extends DenseLargeFloatMatrix3DTest(arg0) {

  protected def createMatrices() {
    A = new DenseLargeFloatMatrix3D(NCOLUMNS, NROWS, NSLICES)
      .viewDice(2, 1, 0)
    B = new DenseLargeFloatMatrix3D(NCOLUMNS, NROWS, NSLICES)
      .viewDice(2, 1, 0)
  }
}
