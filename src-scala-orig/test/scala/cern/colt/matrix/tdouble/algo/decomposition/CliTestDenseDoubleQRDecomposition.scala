package cern.colt.matrix.tdouble.algo.decomposition

import java.util.Random
import cern.colt.matrix.tdouble.DoubleFactory2D
import cern.colt.matrix.tdouble.StrideMatrix2D
import cern.colt.matrix.tdouble.algo.DenseDoubleAlgebra
import cern.colt.matrix.tdouble.algo.DoubleProperty
import cern.colt.matrix.tdouble.impl.DenseColumnDoubleMatrix2D
import cern.jet.math.tdouble.DoubleFunctions
//remove if not needed
import scala.collection.JavaConversions._

object CliTestDenseDoubleQRDecomposition {

  def main(args: Array[String]) {
    for (k <- 0 until 20) {
      val M = 600
      val N = 400
      val NRHS = 5
      val r = new Random(0)
      val A1 = new DenseColumnDoubleMatrix2D(M, N)
      val A2 = new DenseColumnDoubleMatrix2D(M, N)
      val B1 = new DenseColumnDoubleMatrix2D(M, NRHS)
      val B2 = new DenseColumnDoubleMatrix2D(M, NRHS)
      for (i <- 0 until M; j <- 0 until N) {
        A1.setQuick(i, j, 0.5 - r.nextDouble())
        A2.setQuick(i, j, A1.getQuick(i, j))
      }
      for (i <- 0 until M; j <- 0 until NRHS) {
        B1.setQuick(i, j, 0.5 - r.nextDouble())
        B2.setQuick(i, j, B1.getQuick(i, j))
      }
      testQR(A1, A2, B1, B2)
    }
    println("All finished")
    System.exit(0)
  }

  private def testQR(A1: StrideMatrix2D,
      A2: StrideMatrix2D,
      B1: StrideMatrix2D,
      B2: StrideMatrix2D) {
    val M = A1.rows()
    val N = A1.columns()
    val eps = 1e-10
    val qr = new DenseDoubleQRDecomposition(A2)
    val Q = qr.getQ(false)
    val R = qr.getR(false)
    val X = B2.copy()
    qr.solve(X)
    System.out.print("\n")
    System.out.print("------ DenseDoubleQRFactorization tests-------  \n")
    System.out.print(String.format("            Size of the Matrix %d by %d\n", M, N))
    System.out.print("\n")
    System.out.print(" The matrix A is randomly generated for each test.\n")
    System.out.print("============\n")
    System.out.print(String.format(" The relative machine precision (eps) is to be %e \n", eps))
    System.out.print(" Computational tests pass if scaled residuals are less than 10.\n")
    val info_ortho = check_orthogonality(M, Q, eps)
    val info_factorization = checkFactorization(A1, Q, R, eps)
    val info_solution = checkSolution(A1, B1, X.viewPart(0, 0, A1.columns(), X.columns()).copy(), eps)
    if ((info_solution == 0) & (info_factorization == 0) & (info_ortho == 0)) {
      System.out.print("************************************************\n")
      System.out.print(" ---- TESTING DGEQRF + DORMQR + DTRSM .... PASSED !\n")
      System.out.print("************************************************\n")
    } else {
      System.out.print("************************************************\n")
      System.out.print(" ---- TESTING DGEQRF + DORMQR + DTRSM .... FAILED !\n")
      System.out.print("************************************************\n")
    }
  }

  private def check_orthogonality(M: Int, Q: StrideMatrix2D, eps: Double): Int = {
    var alpha: Double = 0.0
    var beta: Double = 0.0
    var normQ: Double = 0.0
    var info_ortho: Int = 0
    alpha = 1.0
    beta = -1.0
    var Id = DoubleFactory2D.dense.identity(Q.columns())
    Id = Q.zMult(Q, Id, alpha, beta, true, false)
    normQ = DenseDoubleAlgebra.DEFAULT.normInfinity(Id)
    System.out.print("============\n")
    System.out.print("Checking the orthogonality of Q \n")
    System.out.print(String.format("||Id-Q'*Q||_oo / (N*eps) = %e\n", normQ / (M * eps)))
    if (normQ / (M * eps) > 10.0) {
      System.out.print("-- Orthogonality is suspicious ! \n")
      info_ortho = 1
    } else {
      System.out.print("-- Orthogonality is CORRECT ! \n")
      info_ortho = 0
    }
    info_ortho
  }

  private def checkFactorization(A1: StrideMatrix2D,
      Q: StrideMatrix2D,
      R: StrideMatrix2D,
      eps: Double): Int = {
    DoubleProperty.DEFAULT.checkDense(A1)
    DoubleProperty.DEFAULT.checkDense(Q)
    DoubleProperty.DEFAULT.checkDense(R)
    val M = A1.rows()
    val N = A1.columns()
    var info_factorization: Int = 0
    var Anorm: Double = 0.0
    var Rnorm: Double = 0.0
    var alpha: Double = 0.0
    var Residual: StrideMatrix2D = null
    var A2 = Q.copy()
    alpha = 1.0
    Residual = A1.copy()
    A2 = A2.zMult(R, null, alpha, 0, false, false)
    Residual.assign(A2, DoubleFunctions.plusMultFirst(-1))
    Rnorm = DenseDoubleAlgebra.DEFAULT.normInfinity(Residual)
    Anorm = DenseDoubleAlgebra.DEFAULT.normInfinity(A1)
    System.out.print("============\n")
    System.out.print("Checking the QR Factorization \n")
    System.out.print(String.format("-- ||QR-A||_oo/(||A||_oo.N.eps) = %e \n", Rnorm / (Anorm * N * eps)))
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
    Anorm = DenseDoubleAlgebra.DEFAULT.normInfinity(A1)
    Bnorm = DenseDoubleAlgebra.DEFAULT.normInfinity(B1)
    A1.zMult(B2, B1, alpha, beta, false, false)
    val Residual = A1.zMult(B1.viewPart(0, 0, A1.rows(), B1.columns()).copy(), null, alpha, beta, true,
      false)
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
