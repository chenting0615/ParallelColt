package cern.colt.matrix.tdouble.algo

import cern.colt.matrix.tdouble.DoubleFactory1D
import cern.colt.matrix.tdouble.StrideMatrix1D
//remove if not needed
import scala.collection.JavaConversions._

object CliTestNormInfinity {

  def main(args: Array[String]) {
    val x1 = DoubleFactory1D.dense.make(Array(1.0, 2.0))
    val x2 = DoubleFactory1D.dense.make(Array(1.0, -2.0))
    val x3 = DoubleFactory1D.dense.make(Array(-1.0, -2.0))
    println(DenseDoubleAlgebra.DEFAULT.normInfinity(x1))
    println(DenseDoubleAlgebra.DEFAULT.normInfinity(x2))
    println(DenseDoubleAlgebra.DEFAULT.normInfinity(x3))
  }
}
