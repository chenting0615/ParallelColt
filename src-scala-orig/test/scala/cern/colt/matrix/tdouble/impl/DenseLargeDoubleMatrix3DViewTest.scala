package cern.colt.matrix.tdouble.impl

//remove if not needed
import scala.collection.JavaConversions._

class DenseLargeDoubleMatrix3DViewTest(arg0: String) extends DenseLargeDoubleMatrix3DTest(arg0) {

  protected def createMatrices() {
    A = new DenseLargeDoubleMatrix3D(NCOLUMNS, NROWS, NSLICES)
      .viewDice(2, 1, 0)
    B = new DenseLargeDoubleMatrix3D(NCOLUMNS, NROWS, NSLICES)
      .viewDice(2, 1, 0)
  }
}
