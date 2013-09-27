package cern.colt.matrix.tdouble.algo.solver

import cern.colt.matrix.tdouble.StrideMatrix1D
import cern.colt.matrix.tdouble.StrideMatrix2D
import cern.jet.math.tdouble.DoubleFunctions
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Iterative Refinement. IR solves the unsymmetric linear system
 * <code>Ax = b</code> using Iterative Refinement (preconditioned Richardson
 * iteration).
 *
 * @author Templates
 */
class DoubleIR(template: StrideMatrix1D) extends AbstractDoubleIterativeSolver {

  /**
   * Vectors for use in the iterative solution process
   */
  private var z: StrideMatrix1D = template.copy()

  private var r: StrideMatrix1D = template.copy()

  def solve(A: StrideMatrix2D, b: StrideMatrix1D, x: StrideMatrix1D): StrideMatrix1D = {
    checkSizes(A, b, x)
    A.zMult(x, r.assign(b), -1, 1, false)
    iter.setFirst()
    while (!iter.converged(r, x)) {
      M.apply(r, z)
      x.assign(z, DoubleFunctions.plus)
      A.zMult(x, r.assign(b), -1, 1, false)
      iter.next()
    }
    x
  }
}
