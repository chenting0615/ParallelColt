package cern.colt.matrix.tdouble.algo.decomposition

import java.util.Random
import cern.colt.matrix.tdouble.StrideMatrix1D
import cern.colt.matrix.tdouble.StrideMatrix2D
import cern.colt.matrix.tdouble.algo.DenseDoubleAlgebra
import cern.colt.matrix.tdouble.algo.DoubleProperty
import cern.colt.matrix.tdouble.algo.SparseDoubleAlgebra
import cern.colt.matrix.tdouble.impl.DenseMatrix1D
import cern.colt.matrix.tdouble.impl.SparseCCDoubleMatrix2D
import cern.jet.math.tdouble.DoubleFunctions
//remove if not needed
import scala.collection.JavaConversions._

object CliTestSparseDoubleCholeskyDecomposition {

  def main(args: Array[String]) {
    val N = 200
    val r = new Random(0)
    val A1 = new SparseCCDoubleMatrix2D(N, N)
    val A2 = new SparseCCDoubleMatrix2D(N, N)
    val B1 = new DenseMatrix1D(N)
    val B2 = new DenseMatrix1D(N)
    for (i <- 0 until N; j <- 0 until N) {
      A1.setQuick(i, j, 0.5 - r.nextDouble())
      A2.setQuick(i, j, A1.getQuick(i, j))
    }
    for (i <- 0 until N) {
      A1.setQuick(i, i, A1.getQuick(i, i) + N)
      A2.setQuick(i, i, A1.getQuick(i, i))
    }
    for (i <- 0 until N; j <- 0 until N) {
      A1.setQuick(i, j, A1.getQuick(j, i))
      A2.setQuick(i, j, A1.getQuick(j, i))
    }
    for (i <- 0 until N) {
      B1.setQuick(i, 0.5 - r.nextDouble())
      B2.setQuick(i, B1.getQuick(i))
    }
    testCholesky(A1, A2, B1, B2)
    System.exit(0)
  }

  private def testCholesky(A1: StrideMatrix2D,
      A2: StrideMatrix2D,
      B1: StrideMatrix1D,
      B2: StrideMatrix1D) {
    val N = A1.rows()
    val eps = 1e-10
    val cf = new SparseDoubleCholeskyDecomposition(A2, 0)
    val L = cf.getL
    val X = B2.copy()
    cf.solve(X)
    System.out.print("\n")
    System.out.print("------ SparseDoubleCholeskyFactorization tests-------  \n")
    System.out.print(String.format("            Size of the Matrix %d by %d\n", N, N))
    System.out.print("\n")
    System.out.print(" The matrix A is randomly generated for each test.\n")
    System.out.print("============\n")
    System.out.print(String.format(" The relative machine precision (eps) is to be %e \n", eps))
    System.out.print(" Computational tests pass if scaled residuals are less than 10.\n")
    val info_factorization = checkFactorization(A1, L, eps)
    val info_solution = checkSolution(A1, B1, X, eps)
    if ((info_solution == 0) & (info_factorization == 0)) {
      System.out.print("***************************************************\n")
      System.out.print(" ---- SparseDoubleCholeskyFactorization tests... PASSED !\n")
      System.out.print("***************************************************\n")
    } else {
      System.err.print("***************************************************\n")
      System.err.print(" ---- SparseDoubleCholeskyFactorization tests... FAILED !\n")
      System.err.print("***************************************************\n")
    }
  }

  private def checkFactorization(A1: StrideMatrix2D, L: StrideMatrix2D, eps: Double): Int = {
    DoubleProperty.DEFAULT.checkSparse(A1)
    DoubleProperty.DEFAULT.checkSparse(L)
    val N = A1.rows()
    var info_factorization: Int = 0
    var Anorm: Double = 0.0
    var Rnorm: Double = 0.0
    var alpha: Double = 0.0
    var Residual: StrideMatrix2D = null
    val L1 = L.copy()
    var L2 = L.copy()
    alpha = 1.0
    Residual = A1.copy()
    L2 = L1.zMult(L2, null, alpha, 0, false, true)
    Residual.assign(L2, DoubleFunctions.plusMultFirst(-1))
    Rnorm = SparseDoubleAlgebra.DEFAULT.normInfinity(Residual)
    Anorm = SparseDoubleAlgebra.DEFAULT.normInfinity(A1)
    System.out.print("============\n")
    System.out.print("Checking the Cholesky Factorization \n")
    System.out.print(String.format("-- ||L'L-A||_oo/(||A||_oo.N.eps) = %e \n", Rnorm / (Anorm * N * eps)))
    if (Rnorm / (Anorm * N * eps) > 10.0) {
      System.out.print("-- Factorization is suspicious ! \n")
      info_factorization = 1
    } else {
      System.out.print("-- Factorization is CORRECT ! \n")
      info_factorization = 0
    }
    info_factorization
  }

  private def checkSolution(A1: StrideMatrix2D,
      B1: StrideMatrix1D,
      B2: StrideMatrix1D,
      eps: Double): Int = {
    DoubleProperty.DEFAULT.checkSparse(A1)
    DoubleProperty.DEFAULT.checkDense(B1)
    DoubleProperty.DEFAULT.checkDense(B2)
    val N = A1.rows()
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
    Rnorm = DenseDoubleAlgebra.DEFAULT.normInfinity(B1)
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
