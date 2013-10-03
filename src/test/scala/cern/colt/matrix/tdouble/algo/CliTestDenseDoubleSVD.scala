package cern.colt.matrix.tdouble.algo

import java.util.Random
import cern.jet.math.tdouble.DoubleFunctions
import cern.colt.matrix.algo.{DoubleProperty, DenseDoubleSingularValueDecomposition}
import cern.colt.matrix.MatrixTypes.{DoubleMatrix2D, DenseDoubleMatrix2D}
import cern.colt.matrix.MatrixOperators._

object CliTestDenseDoubleSVD {

  def main(args: Array[String]) {
    val M = 60
    val N = 40
    val NRHS = 1
    val r = new Random(0)
    val A1 = new DenseDoubleMatrix2D(M, N)
    val A2 = new DenseDoubleMatrix2D(M, N)
    val B1 = new DenseDoubleMatrix2D(M, NRHS)
    val B2 = new DenseDoubleMatrix2D(M, NRHS)
    for (i <- 0 until M; j <- 0 until N) {
      A1.setQuick(i, j, 0.5 - r.nextDouble())
      A2.setQuick(i, j, A1.getQuick(i, j))
    }
    for (i <- 0 until M; j <- 0 until NRHS) {
      B1.setQuick(i, j, 0.5 - r.nextDouble())
      B2.setQuick(i, j, B1.getQuick(i, j))
    }
    testSVD(A1, A2, B1, B2)
  }

  private def testSVD(A1: DoubleMatrix2D,
      A2: DoubleMatrix2D,
      B1: DoubleMatrix2D,
      B2: DoubleMatrix2D) {
    val M = A1.rows
    val N = A1.columns
    val eps = 1e-10
    val svd = new DenseDoubleSingularValueDecomposition(A2, true, false)
    val S = svd.getS
    val V = svd.getV
    val U = svd.getU
    println(svd.toString)
    System.out.print("\n")
    System.out.print("------ DenseDoubleSingularValueDecomposition tests-------  \n")
    System.out.println("            Size of the Matrix " + M + " by " + N)
    System.out.print("\n")
    System.out.print(" The matrix A is randomly generated for each test.\n")
    System.out.print("============\n")
    System.out.println(" The relative machine precision (eps) is to be " + eps)
    System.out.print(" Computational tests pass if scaled residuals are less than 10.\n")
    val info_factorization = checkFactorization(A1, U, S, V, eps)
    if (info_factorization == 0) {
      System.out.print("************************************************\n")
      System.out.print(" ---- TESTING DenseDoubleSingularValueDecomposition .... PASSED !\n")
      System.out.print("************************************************\n")
    } else {
      System.out.print("************************************************\n")
      System.out.print(" ---- TESTING DenseDoubleSingularValueDecomposition .... FAILED !\n")
      System.out.print("************************************************\n")
    }
  }

  private def checkFactorization(A1: DoubleMatrix2D,
      U: DoubleMatrix2D,
      S: DoubleMatrix2D,
      V: DoubleMatrix2D,
      eps: Double): Int = {
    DoubleProperty.DEFAULT.checkDense(A1)
    DoubleProperty.DEFAULT.checkDense(U)
    DoubleProperty.DEFAULT.checkDense(V)
    var info_factorization: Int = 0
    var Anorm: Double = 0.0
    var Rnorm: Double = 0.0
    var alpha: Double = 0.0
    var Residual: DoubleMatrix2D = null
    var A2 = U.copy()
    alpha = 1.0
    Residual = A1.copy()
    A2 = A2.dot(S, null, alpha, 0, transposeSelf=false, transposeOther=false)
    A2 = A2.dot(V, null, alpha, 0, transposeSelf=false, transposeOther=true)
    Residual.assign(A2, DoubleFunctions.plusMultFirst(-1))
    Rnorm = Residual.normInfinity
    Anorm = A1.normInfinity
    System.out.print("============\n")
    System.out.print("Checking the SVD Factorization \n")
    System.out.println("-- ||USV'-A||_oo/(||A||_oo.N.eps) = " + (Rnorm / (Anorm * A1.columns * eps)))
    if (Rnorm / (Anorm * A1.columns * eps) > 10.0) {
      System.out.print("-- Factorization is suspicious ! \n")
      info_factorization = 1
    } else {
      System.out.print("-- Factorization is CORRECT ! \n")
      info_factorization = 0
    }
    info_factorization
  }
}
