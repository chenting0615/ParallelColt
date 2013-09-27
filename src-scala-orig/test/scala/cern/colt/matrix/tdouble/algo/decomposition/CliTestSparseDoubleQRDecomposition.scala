package cern.colt.matrix.tdouble.algo.decomposition

import java.util.Random
import cern.colt.matrix.tdouble.StrideMatrix1D
import cern.colt.matrix.tdouble.StrideMatrix2D
import cern.colt.matrix.tdouble.algo.DenseDoubleAlgebra
import cern.colt.matrix.tdouble.algo.DoubleProperty
import cern.colt.matrix.tdouble.algo.SparseDoubleAlgebra
import cern.colt.matrix.tdouble.impl.DenseMatrix1D
import cern.colt.matrix.tdouble.impl.SparseRCDoubleMatrix2D
//remove if not needed
import scala.collection.JavaConversions._

object CliTestSparseDoubleQRDecomposition {

  def main(args: Array[String]) {
    val M = 280
    val N = 180
    val MN = Math.max(M, N)
    val r = new Random(0)
    val A1 = new SparseRCDoubleMatrix2D(M, N)
    val A2 = new SparseRCDoubleMatrix2D(M, N)
    val B1 = new DenseMatrix1D(MN)
    val B2 = new DenseMatrix1D(MN)
    for (i <- 0 until M; j <- 0 until N) {
      A1.setQuick(i, j, 0.5 - r.nextDouble())
      A2.setQuick(i, j, A1.getQuick(i, j))
    }
    for (i <- 0 until MN) {
      B1.setQuick(i, 0.5 - r.nextDouble())
      B2.setQuick(i, B1.getQuick(i))
    }
    testQR(A1, A2, B1, B2)
  }

  private def testQR(A1: StrideMatrix2D,
      A2: StrideMatrix2D,
      B1: StrideMatrix1D,
      B2: StrideMatrix1D) {
    val M = A1.rows()
    val N = A1.columns()
    val eps = 1e-10
    val qr = new SparseDoubleQRDecomposition(A2, 0)
    val X = B2.copy()
    qr.solve(X)
    System.out.print("\n")
    System.out.print("------ SparseDoubleQRFactorization tests-------  \n")
    System.out.print(String.format("            Size of the Matrix %d by %d\n", M, N))
    System.out.print("\n")
    System.out.print(" The matrix A is randomly generated for each test.\n")
    System.out.print("============\n")
    System.out.print(String.format(" The relative machine precision (eps) is to be %e \n", eps))
    System.out.print(" Computational tests pass if scaled residuals are less than 10.\n")
    val info_solution = checkSolution(A1, B1, X.viewPart(0, A1.columns()).copy(), eps)
    if ((info_solution == 0)) {
      System.out.print("***************************************************\n")
      System.out.print(" ---- SparseDoubleQRFactorization tests... PASSED !\n")
      System.out.print("***************************************************\n")
    } else {
      System.out.print("***************************************************\n")
      System.out.print(" ---- SparseDoubleQRFactorization tests... FAILED !\n")
      System.out.print("***************************************************\n")
    }
  }

  private def checkSolution(A1: StrideMatrix2D,
      B1: StrideMatrix1D,
      B2: StrideMatrix1D,
      eps: Double): Int = {
    DoubleProperty.DEFAULT.checkSparse(A1)
    DoubleProperty.DEFAULT.checkDense(B1)
    DoubleProperty.DEFAULT.checkDense(B2)
    val M = A1.rows()
    val N = A1.columns()
    var info_solution: Int = 0
    var Rnorm: Double = 0.0
    var Anorm: Double = 0.0
    var Xnorm: Double = 0.0
    var Bnorm: Double = 0.0
    var alpha: Double = 0.0
    var beta: Double = 0.0
    alpha = 1.0
    beta = -1.0
    Xnorm = DenseDoubleAlgebra.DEFAULT.normInfinity(B2)
    Anorm = SparseDoubleAlgebra.DEFAULT.normInfinity(A1)
    Bnorm = DenseDoubleAlgebra.DEFAULT.normInfinity(B1)
    A1.zMult(B2, B1, alpha, beta, false)
    val Residual = A1.zMult(B1.viewPart(0, A1.rows()).copy(), null, alpha, beta, true)
    Rnorm = DenseDoubleAlgebra.DEFAULT.normInfinity(Residual)
    System.out.print("============\n")
    System.out.print("Checking the Residual of the solution \n")
    System.out.print(String.format("-- ||Ax-B||_oo/((||A||_oo||x||_oo+||B||_oo).N.eps) = %e \n", Rnorm / ((Anorm * Xnorm + Bnorm) * N * eps)))
    if (Rnorm / ((Anorm * Xnorm + Bnorm) * N * eps) > 10.0) {
      System.out.print("-- The solution is suspicious ! \n")
      info_solution = 1
    } else {
      System.out.print("-- The solution is CORRECT ! \n")
      info_solution = 0
    }
    info_solution
  }
}
