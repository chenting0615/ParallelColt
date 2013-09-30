package cern.colt.matrix.tlong.impl

//remove if not needed
import scala.collection.JavaConversions._

class SparseLongMatrix3DViewTest(arg0: String) extends SparseLongMatrix3DTest(arg0) {

  protected def createMatrices() {
    A = new SparseLongMatrix3D(NCOLUMNS, NROWS, NSLICES).viewDice(2, 1, 0)
    B = new SparseLongMatrix3D(NCOLUMNS, NROWS, NSLICES).viewDice(2, 1, 0)
  }
}
