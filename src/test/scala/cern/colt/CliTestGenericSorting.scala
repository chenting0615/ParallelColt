package cern.colt

import cern.colt.function.tint.IntComparator
import it.unimi.dsi.fastutil.ints.IntComparator

//remove if not needed
import scala.collection.JavaConversions._

object CliTestGenericSorting {

  /**
   * Just a demo.
   */
  def demo1() {
    var x: Array[Int] = null
    var y: Array[Double] = null
    var z: Array[Double] = null
    x = Array(3, 2, 1)
    y = Array(3.0, 2.0, 1.0)
    z = Array(6.0, 7.0, 8.0)
    val swapper = new Swapper() {

      def swap(a: Int, b: Int) {
        var t1: Int = 0
        var t2: Double = 0.0
        var t3: Double = 0.0
        t1 = x(a)
        x(a) = x(b)
        x(b) = t1
        t2 = y(a)
        y(a) = y(b)
        y(b) = t2
        t3 = z(a)
        z(a) = z(b)
        z(b) = t3
      }
    }
    val comp = new IntComparator() {

      def compare(a: Int, b: Int): Int = {
        return if (x(a) == x(b)) 0 else (if (x(a) < x(b)) -1 else 1)
      }
    }
    println("before:")
    println("X=" + Arrays toString x)
    println("Y=" + Arrays toString y)
    println("Z=" + Arrays toString z)
    val from = 0
    val to = x.length
    GenericSorting.quickSort(from, to, comp, swapper)
    println("after:")
    println("X=" + Arrays toString x)
    println("Y=" + Arrays toString y)
    println("Z=" + Arrays toString z)
    println("\n\n")
  }

  /**
   * Just a demo.
   */
  def demo2() {
    var x: Array[Int] = null
    var y: Array[Double] = null
    var z: Array[Double] = null
    x = Array(6, 7, 8, 9)
    y = Array(3.0, 2.0, 1.0, 3.0)
    z = Array(5.0, 4.0, 4.0, 1.0)
    val swapper = new Swapper() {

      def swap(a: Int, b: Int) {
        var t1: Int = 0
        var t2: Double = 0.0
        var t3: Double = 0.0
        t1 = x(a)
        x(a) = x(b)
        x(b) = t1
        t2 = y(a)
        y(a) = y(b)
        y(b) = t2
        t3 = z(a)
        z(a) = z(b)
        z(b) = t3
      }
    }
    val comp = new IntComparator() {

      def compare(a: Int, b: Int): Int = {
        if (y(a) == y(b)) return if (z(a) == z(b)) 0 else (if (z(a) < z(b)) -1 else 1)
        return if (y(a) < y(b)) -1 else 1
      }
    }
    println("before:")
    println("X=" + Arrays toString x)
    println("Y=" + Arrays toString y)
    println("Z=" + Arrays toString z)
    val from = 0
    val to = x.length
    GenericSorting.quickSort(from, to, comp, swapper)
    println("after:")
    println("X=" + Arrays toString x)
    println("Y=" + Arrays toString y)
    println("Z=" + Arrays toString z)
    println("\n\n")
  }

  /**
   * Checks the correctness of the partition method by generating random input
   * parameters and checking whether results are correct.
   */
  def testRandomly(runs: Int) {
    val engine = new cern.jet.random.tdouble.engine.DoubleMersenneTwister()
    val gen = new cern.jet.random.tdouble.DoubleUniform(engine)
    for (run <- 0 until runs) {
      val maxSize = 50
      val size = gen.nextIntFromTo(1, maxSize)
      var from: Int = 0
      var to: Int = 0
      if (size == 0) {
        from = 0
        to = -1
      } else {
        from = gen.nextIntFromTo(0, size - 1)
        to = gen.nextIntFromTo(Math.min(from, size - 1), size - 1)
      }
      val A1 = new cern.colt.matrix.tdouble.impl.DenseMatrix2D(size, size)
      val P1 = A1.viewPart(from, from, size - to, size - to)
      val intervalFrom = gen.nextIntFromTo(size / 2, 2 * size)
      val intervalTo = gen.nextIntFromTo(intervalFrom, 2 * size)
      for (i <- 0 until size; j <- 0 until size) {
        A1.set(i, j, gen.nextIntFromTo(intervalFrom, intervalTo))
      }
      val A2 = A1.copy()
      val P2 = A2.viewPart(from, from, size - to, size - to)
      val c = 0
      val S1 = cern.colt.matrix.tdouble.algo.DoubleSorting.quickSort
        .sort(P1, c)
      val S2 = cern.colt.matrix.tdouble.algo.DoubleSorting.mergeSort
        .sort(P2, c)
      if (!(S1.viewColumn(c) == S2.viewColumn(c))) throw new InternalError()
    }
    println("All tests passed. No bug detected.")
  }

  def main(args: Array[String]) {
    demo1()
    demo2()
    testRandomly(2)
  }
}
