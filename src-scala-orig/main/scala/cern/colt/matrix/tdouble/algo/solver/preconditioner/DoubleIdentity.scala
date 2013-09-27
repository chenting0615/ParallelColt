package cern.colt.matrix.tdouble.algo.solver.preconditioner

import cern.colt.matrix.tdouble.StrideMatrix1D
import cern.colt.matrix.tdouble.StrideMatrix2D
//remove if not needed
import scala.collection.JavaConversions._

class DoubleIdentity extends DoublePreconditioner {

  def apply(b: StrideMatrix1D, x: StrideMatrix1D): StrideMatrix1D = {
    if (x == null) {
      x = b.like()
    }
    x.assign(b)
  }

  def transApply(b: StrideMatrix1D, x: StrideMatrix1D): StrideMatrix1D = {
    if (x == null) {
      x = b.like()
    }
    x.assign(b)
  }

  def setMatrix(A: StrideMatrix2D) {
  }
}
