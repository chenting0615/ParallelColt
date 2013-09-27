package cern.colt.matrix.tdouble.algo.decomposition

import cern.colt.matrix.tdouble.StrideMatrix2D
import cern.colt.matrix.tdouble.algo.DoubleProperty
import cern.colt.matrix.tdouble.impl.DenseColumnDoubleMatrix2D
import cern.colt.matrix.tdouble.impl.DenseMatrix2D
import edu.emory.mathcs.jplasma.tdouble.Dplasma
import java.util.Random
//remove if not needed
import scala.collection.JavaConversions._

object CliTestDenseDoubleCholeskyDecomposition {

  def main(args: Array[String]) {
    for (k <- 0 until 20) {
      val N = 600
      val NRHS = 5
      var r = new Random(0)
      var A1 = new DenseColumnDoubleMatrix2D(N, N)
      var A2 = new DenseColumnDoubleMatrix2D(N, N)
      var B1 = new DenseColumnDoubleMatrix2D(N, NRHS)
      var B2 = new DenseColumnDoubleMatrix2D(N, NRHS)
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
      for (i <- 0 until N; j <- 0 until NRHS) {
        B1.setQuick(i, j, 0.5 - r.nextDouble())
        B2.setQuick(i, j, B1.getQuick(i, j))
      }
      testCholesky(A1, A2, B1, B2)
      A1 = new DenseMatrix2D(N, N)
      A2 = new DenseMatrix2D(N, N)
      B1 = new DenseMatrix2D(N, NRHS)
      B2 = new DenseMatrix2D(N, NRHS)
      r = new Random(0)
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
      for (i <- 0 until N; j <- 0 until NRHS) {
        B1.setQuick(i, j, 0.5 - r.nextDouble())
        B2.setQuick(i, j, B1.getQuick(i, j))
      }
      testCholesky(A1, A2, B1, B2)
    }
    println("All finished")
    System.exit(0)
  }

  private def testCholesky(A1: StrideMatrix2D,
      A2: StrideMatrix2D,
      B1: StrideMatrix2D,
      B2: StrideMatrix2D) {
    val N = A1.rows()
    val eps = 1e-10
    val cf = new DenseDoubleCholeskyDecomposition(A2)
    val Lt = cf.getLtranspose
    val X = B2.copy()
    cf.solve(X)
    System.out.print("\n")
    System.out.print("------ DoubleCholeskyFactorization tests-------  \n")
    System.out.print(String.format("            Size of the Matrix %d by %d\n", N, N))
    System.out.print("\n")
    System.out.print(" The matrix A is randomly generated for each test.\n")
    System.out.print("============\n")
    System.out.print(String.format(" The relative machine precision (eps) is to be %e \n", eps))
    System.out.print(" Computational tests pass if scaled residuals are less than 10.\n")
    val info_factorization = checkFactorization(A1, Lt, eps)
    val info_solution = checkSolution(A1, B1, X, eps)
    if ((info_solution == 0) & (info_factorization == 0)) {
      System.out.print("***************************************************\n")
      System.out.print(" ---- DoubleCholeskyFactorization tests... PASSED !\n")
      System.out.print("***************************************************\n")
    } else {
      System.err.print("***************************************************\n")
      System.err.print(" ---- DoubleCholeskyFactorization tests... FAILED !\n")
      System.err.print("***************************************************\n")
    }
  }

  private def checkFactorization(A1: StrideMatrix2D, A2: StrideMatrix2D, eps: Double): Int = {
    DoubleProperty.DEFAULT.checkDense(A1)
    DoubleProperty.DEFAULT.checkDense(A2)
    val N = A1.rows()
    val LDA = N
    val uplo = Dplasma.PlasmaUpper
    var Anorm: Double = 0.0
    var Rnorm: Double = 0.0
    var alpha: Double = 0.0
    val norm = "I"
    var info_factorization: Int = 0
    var i: Int = 0
    var j: Int = 0
    var A1elems: Array[Double] = null
    var A2elems: Array[Double] = null
    A1elems = if (A1.isInstanceOf[DenseMatrix2D]) A1.viewDice().copy().elements().asInstanceOf[Array[Double]] else A1.copy().elements().asInstanceOf[Array[Double]]
    A2elems = if (A2.isInstanceOf[DenseMatrix2D]) A2.viewDice().copy().elements().asInstanceOf[Array[Double]] else A2.copy().elements().asInstanceOf[Array[Double]]
    val Residual = Array.ofDim[Double](N * N)
    val L1 = Array.ofDim[Double](N * N)
    val L2 = Array.ofDim[Double](N * N)
    val work = Array.ofDim[Double](N)
    alpha = 1.0
    org.netlib.lapack.Dlacpy.dlacpy("ALL", N, N, A1elems, 0, LDA, Residual, 0, N)
    org.netlib.lapack.Dlacpy.dlacpy(Dplasma.lapack_const(Dplasma.PlasmaUpper), N, N, A2elems, 0, LDA,
      L1, 0, N)
    org.netlib.lapack.Dlacpy.dlacpy(Dplasma.lapack_const(Dplasma.PlasmaUpper), N, N, A2elems, 0, LDA,
      L2, 0, N)
    org.netlib.blas.Dtrmm.dtrmm("L", "U", "T", "N", N, N, alpha, L1, 0, N, L2, 0, N)
    i = 0
    while (i < N) {j = 0
    while (j < N) {Residual(j * N + i) = L2(j * N + i) - Residual(j * N + i)j += 1
    }i += 1
    }
    Rnorm = org.netlib.lapack.Dlange.dlange(norm, N, N, Residual, 0, N, work, 0)
    Anorm = org.netlib.lapack.Dlange.dlange(norm, N, N, A1elems, 0, LDA, work, 0)
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
      B1: StrideMatrix2D,
      B2: StrideMatrix2D,
      eps: Double): Int = {
    DoubleProperty.DEFAULT.checkDense(A1)
    DoubleProperty.DEFAULT.checkDense(B1)
    DoubleProperty.DEFAULT.checkDense(B2)
    val N = A1.rows()
    val LDA = N
    val LDB = N
    val NRHS = B1.columns()
    var info_solution: Int = 0
    var Rnorm: Double = 0.0
    var Anorm: Double = 0.0
    var Xnorm: Double = 0.0
    var Bnorm: Double = 0.0
    val norm = "I"
    var alpha: Double = 0.0
    var beta: Double = 0.0
    val work = Array.ofDim[Double](N)
    var A1elems: Array[Double] = null
    var B1elems: Array[Double] = null
    var B2elems: Array[Double] = null
    A1elems = if (A1.isInstanceOf[DenseMatrix2D]) A1.viewDice().copy().elements().asInstanceOf[Array[Double]] else A1.copy().elements().asInstanceOf[Array[Double]]
    B1elems = if (B1.isInstanceOf[DenseMatrix2D]) B1.viewDice().copy().elements().asInstanceOf[Array[Double]] else B1.copy().elements().asInstanceOf[Array[Double]]
    B2elems = if (B2.isInstanceOf[DenseMatrix2D]) B2.viewDice().copy().elements().asInstanceOf[Array[Double]] else B2.copy().elements().asInstanceOf[Array[Double]]
    alpha = 1.0
    beta = -1.0
    Xnorm = org.netlib.lapack.Dlange.dlange(norm, N, NRHS, B2elems, 0, LDB, work, 0)
    Anorm = org.netlib.lapack.Dlange.dlange(norm, N, N, A1elems, 0, LDA, work, 0)
    Bnorm = org.netlib.lapack.Dlange.dlange(norm, N, NRHS, B1elems, 0, LDB, work, 0)
    org.netlib.blas.Dgemm.dgemm("N", "N", N, NRHS, N, alpha, A1elems, 0, LDA, B2elems, 0, LDB, beta,
      B1elems, 0, LDB)
    Rnorm = org.netlib.lapack.Dlange.dlange(norm, N, NRHS, B1elems, 0, LDB, work, 0)
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
