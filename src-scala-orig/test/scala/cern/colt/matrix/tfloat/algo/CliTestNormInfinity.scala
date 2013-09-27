package cern.colt.matrix.tfloat.algo

import cern.colt.matrix.tfloat.FloatFactory1D
import cern.colt.matrix.tfloat.FloatMatrix1D
//remove if not needed
import scala.collection.JavaConversions._

object CliTestNormInfinity {

  def main(args: Array[String]) {
    val x1 = FloatFactory1D.dense.make(Array(1.0f, 2.0f))
    val x2 = FloatFactory1D.dense.make(Array(1.0f, -2.0f))
    val x3 = FloatFactory1D.dense.make(Array(-1.0f, -2.0f))
    println(DenseFloatAlgebra.DEFAULT.normInfinity(x1))
    println(DenseFloatAlgebra.DEFAULT.normInfinity(x2))
    println(DenseFloatAlgebra.DEFAULT.normInfinity(x3))
  }
}
