package cern.jet.stat.quantile

import cern.colt.Timer
import cern.colt.list.tdouble.DoubleArrayList
import cern.colt.list.tint.IntArrayList
import cern.jet.stat.Utils
import cern.jet.stat.tdouble.quantile.DoubleBuffer
import cern.jet.stat.tdouble.quantile.DoubleQuantileFinder
import cern.jet.stat.tdouble.quantile.DoubleQuantileFinderFactory
import cern.jet.stat.tdouble.quantile.ExactDoubleQuantileFinder
import it.unimi.dsi.fastutil.doubles.DoubleArrayList
import it.unimi.dsi.fastutil.ints.IntArrayList

//remove if not needed
import scala.collection.JavaConversions._

object CliTestQuantileFinder {

  /**
   * Finds the first and last indexes of a specific element within a sorted
   * list.
   *
   * @return int[]
   * @param list
   *            cern.colt.list.DoubleArrayList
   * @param element
   *            the element to search for
   */
  protected def binaryMultiSearch(list: DoubleArrayList, element: Double): IntArrayList = {
    val index = list.binarySearch(element)
    if (index < 0) return null
    var from = index - 1
    while (from >= 0 && list.get(from) == element) from -= 1
    from += 1
    var to = index + 1
    while (to < list.size && list.get(to) == element) to += 1
    to -= 1
    new IntArrayList(Array(from, to))
  }

  /**
   * Observed epsilon
   */
  def epsilon(size: Int, phi: Double, rank: Double): Double = {
    val s = size
    Math.abs(rank / s - phi)
  }

  /**
   * Observed epsilon
   */
  def epsilon(sortedList: DoubleArrayList, phi: Double, element: Double): Double = {
    val rank = cern.jet.stat.tdouble.DoubleDescriptive.rankInterpolated(sortedList, element)
    epsilon(sortedList.size, phi, rank)
  }

  /**
   * Observed epsilon
   */
  def epsilon(sortedList: DoubleArrayList, finder: DoubleQuantileFinder, phi: Double): Double = {
    val element = finder.quantileElements(new DoubleArrayList(Array(phi)))
      .get(0)
    epsilon(sortedList, phi, element)
  }

  def main(args: Array[String]) {
    testBestBandKCalculation(args)
  }

  /**
   * This method was created in VisualAge.
   *
   * @return double[]
   * @param values
   *            cern.it.hepodbms.primitivearray.DoubleArrayList
   * @param phis
   *            double[]
   */
  def observedEpsilonAtPhi(phi: Double, exactFinder: ExactDoubleQuantileFinder, approxFinder: DoubleQuantileFinder): Double = {
    val N = exactFinder.size.toInt
    val exactRank = Utils.epsilonCeiling(phi * N).toInt - 1
    exactFinder.quantileElements(new DoubleArrayList(Array(phi)))
      .get(0)
    val approxElement = approxFinder.quantileElements(new DoubleArrayList(Array(phi)))
      .get(0)
    val approxRanks = binaryMultiSearch(exactFinder.buffer, approxElement)
    val from = approxRanks.get(0)
    val to = approxRanks.get(1)
    var distance: Int = 0
    distance = if (from <= exactRank && exactRank <= to) 0 else if (from > exactRank) Math.abs(from - exactRank) else Math.abs(exactRank - to)
    val epsilon = distance.toDouble / N.toDouble
    epsilon
  }

  /**
   * This method was created in VisualAge.
   *
   * @return double[]
   * @param values
   *            cern.it.hepodbms.primitivearray.DoubleArrayList
   * @param phis
   *            double[]
   */
  def observedEpsilonsAtPhis(phis: DoubleArrayList,
      exactFinder: ExactDoubleQuantileFinder,
      approxFinder: DoubleQuantileFinder,
      desiredEpsilon: Double): DoubleArrayList = {
    val epsilons = new DoubleArrayList(phis.size)
    var i = phis.size
    while (i >= 0) {
      val epsilon = observedEpsilonAtPhi(phis.get(i), exactFinder, approxFinder)
      epsilons.add(epsilon)
      if (epsilon > desiredEpsilon) println("Real epsilon = " + epsilon + " is larger than desired by " +
        (epsilon - desiredEpsilon))
    }
    epsilons
  }

  /**
   * Not yet commented.
   */
  def test() {
    val args = Array.ofDim[String](20)
    val size = "10000"
    args(0) = size
    val b = "12"
    args(1) = b
    val k = "2290"
    args(2) = k
    val enableLogging = "log"
    args(3) = enableLogging
    val chunks = "10"
    args(4) = chunks
    val computeExactQuantilesAlso = "exact"
    args(5) = computeExactQuantilesAlso
    val doShuffle = "shuffle"
    args(6) = doShuffle
    val epsilon = "0.001"
    args(7) = epsilon
    val delta = "0.0001"
    args(8) = delta
    val quantiles = "1"
    args(9) = quantiles
    val max_N = "-1"
    args(10) = max_N
    testQuantileCalculation(args)
  }

  /**
   * This method was created in VisualAge.
   */
  def testBestBandKCalculation(args: Array[String]) {
    val quantiles = Array(100, 10000)
    val sizes = Array(Long.MAX_VALUE, 1000000, 10000000, 100000000)
    val deltas = Array(0.0, 0.1, 0.00001)
    val epsilons = Array(0.0, 0.1, 0.01, 0.001, 0.0001, 0.00001, 0.000001)
    println("\n\n")
    println("mem [Math.round(elements/1000.0)]")
    println("***********************************")
    val timer = new Timer().start()
    for (q <- 0 until quantiles.length) {
      val p = quantiles(q)
      println("------------------------------")
      println("computing for p = " + p)
      for (s <- 0 until sizes.length) {
        val N = sizes(s)
        println("   ------------------------------")
        println("   computing for N = " + N)
        for (e <- 0 until epsilons.length) {
          val epsilon = epsilons(e)
          println("      ------------------------------")
          println("      computing for e = " + epsilon)
          for (d <- 0 until deltas.length) {
            val delta = deltas(d)
            for (knownCounter <- 0 until 2) {
              var known_N: Boolean = false
              known_N = if (knownCounter == 0) true else false
              val finder = DoubleQuantileFinderFactory.newDoubleQuantileFinder(known_N, N, epsilon, delta,
                p, null)
              val knownStr = if (known_N) "  known" else "unknown"
              var mem = finder.totalMemory()
              if (mem == 0) mem = N
              System.out.print("         (known, d)=(" + knownStr + ", " + delta + ") --> ")
              System.out.print("(MB,mem")
              System.out.print(")=(" + mem * 8.0 / 1024.0 / 1024.0 + ",  " + mem / 1000.0 +
                ",  " +
                Math.round(mem * 8.0 / 1024.0 / 1024.0))
              println(")")
            }
          }
        }
      }
    }
    timer.stop().display()
  }

  /**
   * This method was created in VisualAge.
   */
  def testLocalVarDeclarationSpeed(size: Int) {
    println("free=" + Runtime.getRuntime.freeMemory())
    println("total=" + Runtime.getRuntime.totalMemory())
    val timer = new Timer().start()
    var buffer: DoubleBuffer = null
    var `val`: Int = 0
    var f: Double = 0.0
    var j: Int = 0
    for (i <- 0 until size) {
      j = 0
      while (j < size) {
        buffer = null
        `val` = 10
        f = 1.0f
        j += 1
      }
    }
    println(timer.stop())
    println("free=" + Runtime.getRuntime.freeMemory())
    println("total=" + Runtime.getRuntime.totalMemory())
  }

  /**
   */
  def testQuantileCalculation(args: Array[String]) {
    val size = Integer.parseInt(args(0))
    val b = Integer.parseInt(args(1))
    val k = Integer.parseInt(args(2))
    val chunks = Integer.parseInt(args(4))
    val computeExactQuantilesAlso = args(5) == "exact"
    val doShuffle = args(6) == "shuffle"
    val epsilon = new java.lang.Double(args(7)).doubleValue()
    val delta = new java.lang.Double(args(8)).doubleValue()
    val quantiles = Integer.parseInt(args(9))
    val max_N = Long.parseLong(args(10))
    println("free=" + Runtime.getRuntime.freeMemory())
    println("total=" + Runtime.getRuntime.totalMemory())
    val phis = Array(0.001, 0.01, 0.1, 0.5, 0.9, 0.99, 0.999, 1.0)
    val timer = new Timer()
    val timer2 = new Timer()
    var approxFinder: DoubleQuantileFinder = null
    approxFinder = DoubleQuantileFinderFactory.newDoubleQuantileFinder(false, max_N, epsilon, delta,
      quantiles, null)
    println(approxFinder)
    val exactFinder = DoubleQuantileFinderFactory.newDoubleQuantileFinder(false, -1, 0.0, delta, quantiles,
      null)
    println(exactFinder)
    val list = new DoubleArrayList(size)
    for (chunk <- 0 until chunks) {
      list.setSize(0)
      val d = chunk * size
      timer2.start()
      for (i <- 0 until size) {
        list.add((i + d))
      }
      timer2.stop()
      if (doShuffle) {
        val timer3 = new Timer().start()
        list.shuffle()
        println("shuffling took ")
        timer3.stop().display()
      }
      timer.start()
      approxFinder.addAllOf(list)
      timer.stop()
      if (computeExactQuantilesAlso) {
        exactFinder.addAllOf(list)
      }
    }
    println("list.add() took" + timer2)
    println("approxFinder.add() took" + timer)
    timer.reset().start()
    val approxQuantiles = approxFinder.quantileElements(new DoubleArrayList(phis))
    timer.stop().display()
    println("Phis=" + new DoubleArrayList(phis))
    println("ApproxQuantiles=" + approxQuantiles)
    if (computeExactQuantilesAlso) {
      println("Comparing with exact quantile computation...")
      timer.reset().start()
      val exactQuantiles = exactFinder.quantileElements(new DoubleArrayList(phis))
      timer.stop().display()
      println("ExactQuantiles=" + exactQuantiles)
      val observedEpsilons = observedEpsilonsAtPhis(new DoubleArrayList(phis), exactFinder.asInstanceOf[ExactDoubleQuantileFinder],
        approxFinder, epsilon)
      println("observedEpsilons=" + observedEpsilons)
      val element = 1000.0f
      println("exact phi(" + element + ")=" + exactFinder.phi(element))
      println("apprx phi(" + element + ")=" + approxFinder.phi(element))
      println("exact elem(phi(" + element + "))=" +
        exactFinder.quantileElements(new DoubleArrayList(Array(exactFinder.phi(element)))))
      println("apprx elem(phi(" + element + "))=" +
        approxFinder.quantileElements(new DoubleArrayList(Array(approxFinder.phi(element)))))
    }
  }

  /**
   * Not yet commented.
   */
  def testRank() {
    val list = new DoubleArrayList(Array(1.0f, 5.0f, 5.0f, 5.0f, 7.0f, 10.f))
  }
}
