package cern.colt.matrix.tfloat.algo

import cern.colt.matrix.tfloat.FloatFactory2D
import cern.colt.matrix.tfloat.FloatMatrix2D
//remove if not needed
import scala.collection.JavaConversions._

object CliTestQR {

  def main(args: Array[String]) {
    var xmatrix: FloatMatrix2D = null
    var ymatrix: FloatMatrix2D = null
    var zmatrix: FloatMatrix2D = null
    var myfactory: FloatFactory2D = null
    myfactory = FloatFactory2D.dense
    xmatrix = myfactory.make(8, 2)
    ymatrix = myfactory.make(8, 1)
    xmatrix.set(0, 0, 1)
    xmatrix.set(1, 0, 1)
    xmatrix.set(2, 0, 1)
    xmatrix.set(3, 0, 1)
    xmatrix.set(4, 0, 1)
    xmatrix.set(5, 0, 1)
    xmatrix.set(6, 0, 1)
    xmatrix.set(7, 0, 1)
    xmatrix.set(0, 1, 80)
    xmatrix.set(1, 1, 220)
    xmatrix.set(2, 1, 140)
    xmatrix.set(3, 1, 120)
    xmatrix.set(4, 1, 180)
    xmatrix.set(5, 1, 100)
    xmatrix.set(6, 1, 200)
    xmatrix.set(7, 1, 160)
    ymatrix.set(0, 0, 0.6f)
    ymatrix.set(1, 0, 6.70f)
    ymatrix.set(2, 0, 5.30f)
    ymatrix.set(3, 0, 4.00f)
    ymatrix.set(4, 0, 6.55f)
    ymatrix.set(5, 0, 2.15f)
    ymatrix.set(6, 0, 6.60f)
    ymatrix.set(7, 0, 5.75f)
    val myAlgebra = new DenseFloatAlgebra()
    zmatrix = myAlgebra.solve(xmatrix, ymatrix)
    System.err.println(xmatrix)
    System.err.println(ymatrix)
    System.err.println(zmatrix)
  }
}
