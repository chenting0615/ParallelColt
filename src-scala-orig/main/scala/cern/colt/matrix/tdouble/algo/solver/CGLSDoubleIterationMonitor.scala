package cern.colt.matrix.tdouble.algo.solver

import cern.colt.matrix.tdouble.StrideMatrix1D
//remove if not needed
import scala.collection.JavaConversions._

class CGLSDoubleIterationMonitor extends DefaultDoubleIterationMonitor {

  protected def convergedI(r: Double): Boolean = {
    if (isFirst) initR = r
    if (r <= rtol) return true
    if (r > dtol * initR) throw new IterativeSolverDoubleNotConvergedException(DoubleNotConvergedException.Reason.Divergence,
      this)
    if (iter >= maxIter) throw new IterativeSolverDoubleNotConvergedException(DoubleNotConvergedException.Reason.Iterations,
      this)
    if (Double.isNaN(r)) throw new IterativeSolverDoubleNotConvergedException(DoubleNotConvergedException.Reason.Divergence,
      this)
    false
  }

  protected def convergedI(r: Double, x: StrideMatrix1D): Boolean = convergedI(r)
}
