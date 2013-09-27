package cern.colt.matrix.tdouble.algo

import cern.colt.matrix.tdouble.StrideMatrix2D
import cern.colt.matrix.tdouble.Matrix2DProcedure
import cern.colt.matrix.tdouble.DoubleMatrix3D
import cern.colt.matrix.tdouble.DoubleMatrix3DProcedure
import DoubleStencil._
//remove if not needed
import scala.collection.JavaConversions._

object DoubleStencil {

  /**
   * 27 point stencil operation. Applies a function to a moving
   * <tt>3 x 3 x 3</tt> window.
   *
   * @param A
   *            the matrix to operate on.
   * @param function
   *            the function to be applied to each window.
   * @param maxIterations
   *            the maximum number of times the stencil shall be applied to
   *            the matrix. Should be a multiple of 2 because two iterations
   *            are always done in one atomic step.
   * @param hasConverged
   *            Convergence condition; will return before maxIterations are
   *            done when <tt>hasConverged.apply(A)==true</tt>. Set this
   *            parameter to <tt>null</tt> to indicate that no convergence
   *            checks shall be made.
   * @param convergenceIterations
   *            the number of iterations to pass between each convergence
   *            check. (Since a convergence may be expensive, you may want to
   *            do it only every 2,4 or 8 iterations.)
   * @return the number of iterations actually executed.
   */
  def stencil27(A: DoubleMatrix3D,
      function: cern.colt.function.tdouble.Double27Function,
      maxIterations: Int,
      hasConverged: DoubleMatrix3DProcedure,
      convergenceIterations: Int): Int = {
    val B = A.copy()
    if (convergenceIterations <= 1) convergenceIterations = 2
    if (convergenceIterations % 2 != 0) convergenceIterations += 1
    var i = 0
    while (i < maxIterations) {
      A.zAssign27Neighbors(B, function)
      B.zAssign27Neighbors(A, function)
      i = i + 2
      if (i % convergenceIterations == 0 && hasConverged != null) {
        if (hasConverged.apply(A)) return i
      }
    }
    i
  }

  /**
   * 9 point stencil operation. Applies a function to a moving <tt>3 x 3</tt>
   * window.
   *
   * @param A
   *            the matrix to operate on.
   * @param function
   *            the function to be applied to each window.
   * @param maxIterations
   *            the maximum number of times the stencil shall be applied to
   *            the matrix. Should be a multiple of 2 because two iterations
   *            are always done in one atomic step.
   * @param hasConverged
   *            Convergence condition; will return before maxIterations are
   *            done when <tt>hasConverged.apply(A)==true</tt>. Set this
   *            parameter to <tt>null</tt> to indicate that no convergence
   *            checks shall be made.
   * @param convergenceIterations
   *            the number of iterations to pass between each convergence
   *            check. (Since a convergence may be expensive, you may want to
   *            do it only every 2,4 or 8 iterations.)
   * @return the number of iterations actually executed.
   */
  def stencil9(A: StrideMatrix2D,
      function: cern.colt.function.tdouble.Double9Function,
      maxIterations: Int,
      hasConverged: Matrix2DProcedure,
      convergenceIterations: Int): Int = {
    val B = A.copy()
    if (convergenceIterations <= 1) convergenceIterations = 2
    if (convergenceIterations % 2 != 0) convergenceIterations += 1
    var i = 0
    while (i < maxIterations) {
      A.zAssign8Neighbors(B, function)
      B.zAssign8Neighbors(A, function)
      i = i + 2
      if (i % convergenceIterations == 0 && hasConverged != null) {
        if (hasConverged.apply(A)) return i
      }
    }
    i
  }
}

/**
 * Stencil operations. For efficient finite difference operations. Applies a
 * function to a moving <tt>3 x 3</tt> or <tt>3 x 3 x 3</tt> window. Build on
 * top of <tt>matrix.zAssignXXXNeighbors(...)</tt>. You can specify how many
 * iterations shall at most be done, a convergence condition when iteration
 * shall be terminated, and how many iterations shall pass between convergence
 * checks. Always does two iterations at a time for efficiency. These class is
 * for convencience and efficiency.
 *
 * @author wolfgang.hoschek@cern.ch
 * @version 1.0, 01/02/2000
 */
class DoubleStencil protected () extends AnyRef
