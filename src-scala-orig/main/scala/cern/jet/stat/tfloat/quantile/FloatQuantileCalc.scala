package cern.jet.stat.tfloat.quantile

//remove if not needed
import scala.collection.JavaConversions._

object FloatQuantileCalc {

  /**
   * Efficiently computes the binomial coefficient, often also referred to as
   * "n over k" or "n choose k". The binomial coefficient is defined as
   * n!/((n-k)!*k!). Tries to avoid numeric overflows.
   *
   * @return the binomial coefficient.
   */
  def binomial(n: Long, k: Long): Float = {
    if (k == 0 || k == n) {
      return 1.0f
    }
    if (k > n / 2.0) k = n - k
    var binomial = 1.0f
    val N = n - k + 1
    var i = k
    while (i > 0) {
      binomial *= N += 1.toFloat / (i -= 1).toFloat
    }
    binomial
  }

  /**
   * Returns the smallest <code>long &gt;= value</code>. <dt>Examples:
   * <code>1.0 -> 1, 1.2 -> 2, 1.9 -> 2</code>. This method is safer than
   * using (long) Math.ceil(value), because of possible rounding error.
   */
  def ceiling(value: Float): Long = Math.round(Math.ceil(value))

  /**
   * Computes the number of buffers and number of values per buffer such that
   * quantiles can be determined with an approximation error no more than
   * epsilon with a certain probability.
   *
   * Assumes that quantiles are to be computed over N values. The required
   * sampling rate is computed and stored in the first element of the provided
   * <tt>returnSamplingRate</tt> array, which, therefore must be at least of
   * length 1.
   *
   * @param N
   *            the number of values over which quantiles shall be computed
   *            (e.g <tt>10^6</tt>).
   * @param epsilon
   *            the approximation error which is guaranteed not to be exceeded
   *            (e.g. <tt>0.001</tt>) (<tt>0 &lt;= epsilon &lt;= 1</tt>). To
   *            get exact result, set <tt>epsilon=0.0</tt>;
   * @param delta
   *            the probability that the approximation error is more than than
   *            epsilon (e.g. <tt>0.0001</tt>) (<tt>0 &lt;= delta &lt;= 1</tt>
   *            ). To avoid probabilistic answers, set <tt>delta=0.0</tt>.
   * @param quantiles
   *            the number of quantiles to be computed (e.g. <tt>100</tt>) (
   *            <tt>quantiles &gt;= 1</tt>). If unknown in advance, set this
   *            number large, e.g. <tt>quantiles &gt;= 10000</tt>.
   * @param samplingRate
   *            a <tt>float[1]</tt> where the sampling rate is to be filled
   *            in.
   * @return <tt>long[2]</tt> - <tt>long[0]</tt>=the number of buffers,
   *         <tt>long[1]</tt>=the number of elements per buffer,
   *         <tt>returnSamplingRate[0]</tt>=the required sampling rate.
   */
  def known_N_compute_B_and_K(N: Long, 
      epsilon: Float, 
      delta: Float, 
      quantiles: Int, 
      returnSamplingRate: Array[Float]): Array[Long] = {
    if (delta > 0.0) {
      return known_N_compute_B_and_K_slow(N, epsilon, delta, quantiles, returnSamplingRate)
    }
    returnSamplingRate(0) = 1.0f
    known_N_compute_B_and_K_quick(N, epsilon)
  }

  /**
   * Computes the number of buffers and number of values per buffer such that
   * quantiles can be determined with a <b>guaranteed</b> approximation error
   * no more than epsilon. Assumes that quantiles are to be computed over N
   * values.
   *
   * @return <tt>long[2]</tt> - <tt>long[0]</tt>=the number of buffers,
   *         <tt>long[1]</tt>=the number of elements per buffer.
   * @param N
   *            the anticipated number of values over which quantiles shall be
   *            determined.
   * @param epsilon
   *            the approximation error which is guaranteed not to be exceeded
   *            (e.g. <tt>0.001</tt>) (<tt>0 &lt;= epsilon &lt;= 1</tt>). To
   *            get exact result, set <tt>epsilon=0.0</tt>;
   */
  protected def known_N_compute_B_and_K_quick(N: Long, epsilon: Float): Array[Long] = {
    if (epsilon <= 0.0) {
      val result = Array.ofDim[Long](2)
      result(0) = 1
      result(1) = N
      return result
    }
    val maxBuffers = 50
    val maxHeight = 50
    val N_float = N
    val c = N_float * epsilon * 2.0f
    val heightMaximums = Array.ofDim[Int](maxBuffers - 1)
    var b = 2
    while (b <= maxBuffers) {
      var h = 3
      while (h <= maxHeight && 
        (h - 2) * Math.round(binomial(b + h - 2, h - 1)).toFloat - 
        (Math.round(binomial(b + h - 3, h - 3))) + 
        (Math.round(binomial(b + h - 3, h - 2))) - 
        c > 
        0.0) {
        h += 1
      }
      while (h <= maxHeight && 
        (h - 2) * Math.round(binomial(b + h - 2, h - 1)).toFloat - 
        (Math.round(binomial(b + h - 3, h - 3))) + 
        (Math.round(binomial(b + h - 3, h - 2))) - 
        c <= 
        0.0) {
        h += 1
      }
      h -= 1
      var hMax: Int = 0
      hMax = if (h >= maxHeight && 
        (h - 2) * Math.round(binomial(b + h - 2, h - 1)).toFloat - 
        (Math.round(binomial(b + h - 3, h - 3))) + 
        (Math.round(binomial(b + h - 3, h - 2))) - 
        c > 
        0.0) Integer.MIN_VALUE else h
      heightMaximums(b - 2) = hMax
      b += 1
    }
    val kMinimums = Array.ofDim[Long](maxBuffers - 1)
    var b = 2
    while (b <= maxBuffers) {
      val h = heightMaximums(b - 2)
      var kMin = Long.MAX_VALUE
      if (h > Integer.MIN_VALUE) {
        val value = (Math.round(binomial(b + h - 2, h - 1)))
        val tmpK = ceiling(N_float / value)
        if (tmpK <= Long.MAX_VALUE) {
          kMin = tmpK
        }
      }
      kMinimums(b - 2) = kMin
      b += 1
    }
    var multMin = Long.MAX_VALUE
    var minB = -1
    var b = 2
    while (b <= maxBuffers) {
      if (kMinimums(b - 2) < Long.MAX_VALUE) {
        val mult = (b) * (kMinimums(b - 2))
        if (mult < multMin) {
          multMin = mult
          minB = b
        }
      }
      b += 1
    }
    var b: Long = 0l
    var k: Long = 0l
    if (minB != -1) {
      b = minB
      k = kMinimums(minB - 2)
    } else {
      b = 1
      k = N
    }
    val result = Array.ofDim[Long](2)
    result(0) = b
    result(1) = k
    result
  }

  /**
   * Computes the number of buffers and number of values per buffer such that
   * quantiles can be determined with an approximation error no more than
   * epsilon with a certain probability. Assumes that quantiles are to be
   * computed over N values. The required sampling rate is computed and stored
   * in the first element of the provided <tt>returnSamplingRate</tt> array,
   * which, therefore must be at least of length 1.
   *
   * @param N
   *            the anticipated number of values over which quantiles shall be
   *            computed (e.g 10^6).
   * @param epsilon
   *            the approximation error which is guaranteed not to be exceeded
   *            (e.g. <tt>0.001</tt>) (<tt>0 &lt;= epsilon &lt;= 1</tt>). To
   *            get exact result, set <tt>epsilon=0.0</tt>;
   * @param delta
   *            the probability that the approximation error is more than than
   *            epsilon (e.g. <tt>0.0001</tt>) (<tt>0 &lt;= delta &lt;= 1</tt>
   *            ). To avoid probabilistic answers, set <tt>delta=0.0</tt>.
   * @param quantiles
   *            the number of quantiles to be computed (e.g. <tt>100</tt>) (
   *            <tt>quantiles &gt;= 1</tt>). If unknown in advance, set this
   *            number large, e.g. <tt>quantiles &gt;= 10000</tt>.
   * @param samplingRate
   *            a <tt>float[1]</tt> where the sampling rate is to be filled
   *            in.
   * @return <tt>long[2]</tt> - <tt>long[0]</tt>=the number of buffers,
   *         <tt>long[1]</tt>=the number of elements per buffer,
   *         <tt>returnSamplingRate[0]</tt>=the required sampling rate.
   */
  protected def known_N_compute_B_and_K_slow(N: Long, 
      epsilon: Float, 
      delta: Float, 
      quantiles: Int, 
      returnSamplingRate: Array[Float]): Array[Long] = {
    if (epsilon <= 0.0) {
      val result = Array.ofDim[Long](2)
      result(0) = 1
      result(1) = N
      returnSamplingRate(0) = 1.0f
      return result
    }
    val maxBuffers = 50
    val maxHeight = 50
    val N_float = N
    var ret_b = 1
    var ret_k = N
    var sampling_rate = 1.0f
    var memory = N
    val logarithm = Math.log(2.0 * quantiles / delta).toFloat
    val c = 2.0f * epsilon * N_float
    for (b <- 2 until maxBuffers; h <- 3 until maxHeight) {
      val binomial = binomial(b + h - 2, h - 1)
      val tmp = ceiling(N_float / binomial)
      if ((b * tmp < memory) && 
        ((h - 2) * binomial - binomial(b + h - 3, h - 3) + binomial(b + h - 3, h - 2) <= 
        c)) {
        ret_k = tmp
        ret_b = b
        memory = ret_k * b
        sampling_rate = 1.0f
      }
      if (delta > 0.0) {
        val t = (h - 2) * binomial(b + h - 2, h - 1) - binomial(b + h - 3, h - 3) + 
          binomial(b + h - 3, h - 2)
        val u = logarithm / epsilon
        val v = binomial(b + h - 2, h - 1)
        val w = (logarithm / (2.0 * epsilon * epsilon)).toFloat
        val x = (0.5 + 0.5 * Math.sqrt(1.0 + 4.0 * t / u)).toFloat
        val k = ceiling(w * x * x / v)
        if (b * k < memory) {
          ret_k = k
          ret_b = b
          memory = b * k
          sampling_rate = (N_float * 2.0 * epsilon * epsilon / logarithm).toFloat
        }
      }
    }
    val result = Array.ofDim[Long](2)
    result(0) = ret_b
    result(1) = ret_k
    returnSamplingRate(0) = sampling_rate
    result
  }

  def main(args: Array[String]) {
    test_B_and_K_Calculation(args)
  }

  /**
   * Computes b and k for different parameters.
   */
  def test_B_and_K_Calculation(args: Array[String]) {
    var known_N: Boolean = false
    known_N = if (args == null) false else new java.lang.Boolean(args(0)).booleanValue()
    val quantiles = Array(1, 1000)
    var sizes = Array(100000, 1000000, 10000000, 1000000000)
    val deltas = Array(0.0f, 0.001f, 0.0001f, 0.00001f)
    val epsilons = Array(0.0f, 0.1f, 0.05f, 0.01f, 0.005f, 0.001f, 0.0000001f)
    if (!known_N) sizes = Array(0)
    println("\n\n")
    if (known_N) println("Computing b's and k's for KNOWN N") else println("Computing b's and k's for UNKNOWN N")
    println("mem [elements/1024]")
    println("***********************************")
    for (q <- 0 until quantiles.length) {
      val p = quantiles(q)
      println("------------------------------")
      println("computing for p = " + p)
      for (s <- 0 until sizes.length) {
        val N = sizes(s)
        println("   ------------------------------")
        println("   computing for N = " + N)
        for (d <- 0 until deltas.length) {
          val delta = deltas(d)
          println("      ------------------------------")
          println("      computing for delta = " + delta)
          for (e <- 0 until epsilons.length) {
            val epsilon = epsilons(e)
            val returnSamplingRate = Array.ofDim[Float](1)
            var result: Array[Long] = null
            result = if (known_N) known_N_compute_B_and_K(N, epsilon, delta, p, returnSamplingRate) else unknown_N_compute_B_and_K(epsilon, 
              delta, p)
            val b = result(0)
            val k = result(1)
            System.out.print("         (e,d,N,p)=(" + epsilon + "," + delta + "," + 
              N + 
              "," + 
              p + 
              ") --> ")
            System.out.print("(b,k,mem")
            if (known_N) System.out.print(",sampling")
            System.out.print(")=(" + b + "," + k + "," + (b * k / 1024))
            if (known_N) System.out.print("," + returnSamplingRate(0))
            println(")")
          }
        }
      }
    }
  }

  /**
   * Computes the number of buffers and number of values per buffer such that
   * quantiles can be determined with an approximation error no more than
   * epsilon with a certain probability.
   *
   * @param epsilon
   *            the approximation error which is guaranteed not to be exceeded
   *            (e.g. <tt>0.001</tt>) (<tt>0 &lt;= epsilon &lt;= 1</tt>). To
   *            get exact results, set <tt>epsilon=0.0</tt>;
   * @param delta
   *            the probability that the approximation error is more than than
   *            epsilon (e.g. <tt>0.0001</tt>) (<tt>0 &lt;= delta &lt;= 1</tt>
   *            ). To get exact results, set <tt>delta=0.0</tt>.
   * @param quantiles
   *            the number of quantiles to be computed (e.g. <tt>100</tt>) (
   *            <tt>quantiles &gt;= 1</tt>). If unknown in advance, set this
   *            number large, e.g. <tt>quantiles &gt;= 10000</tt>.
   * @return <tt>long[3]</tt> - <tt>long[0]</tt>=the number of buffers,
   *         <tt>long[1]</tt>=the number of elements per buffer,
   *         <tt>long[2]</tt>=the tree height where sampling shall start.
   */
  def unknown_N_compute_B_and_K(epsilon: Float, delta: Float, quantiles: Int): Array[Long] = {
    if (epsilon <= 0.0 || delta <= 0.0) {
      val result = Array.ofDim[Long](3)
      result(0) = 1
      result(1) = Long.MAX_VALUE
      result(2) = Long.MAX_VALUE
      return result
    }
    var max_b = 50
    var max_h = 50
    var max_H = 50
    var max_Iterations = 2
    var best_b = Long.MAX_VALUE
    var best_k = Long.MAX_VALUE
    var best_h = Long.MAX_VALUE
    var best_memory = Long.MAX_VALUE
    val pow = Math.pow(2.0, max_H).toFloat
    val logDelta = (Math.log(2.0 / (delta / quantiles)) / (2.0 * epsilon * epsilon)).toFloat
    while (best_b == Long.MAX_VALUE && max_Iterations -= 1 > 0) {
      var b = 2
      while (b <= max_b) {
        var h = 2
        while (h <= max_h) {
          val Ld = binomial(b + h - 2, h - 1)
          val Ls = binomial(b + h - 3, h - 1)
          val c = (logDelta / Math.min(Ld, 8.0 * Ls / 3.0)).toFloat
          val beta = Ld / Ls
          val cc = ((beta - 2.0) * (max_H - 2.0) / (beta + pow - 2.0)).toFloat
          val d = ((h + 3 + cc) / (2.0 * epsilon)).toFloat
          val f = c * c + 4.0f * c * d
          if (f < 0.0) //continue
          val root = Math.sqrt(f).toFloat
          val alpha_one = ((c + 2.0 * d + root) / (2.0 * d)).toFloat
          val alpha_two = ((c + 2.0 * d - root) / (2.0 * d)).toFloat
          var alpha_one_OK = false
          var alpha_two_OK = false
          if (0.0 < alpha_one && alpha_one < 1.0) alpha_one_OK = true
          if (0.0 < alpha_two && alpha_two < 1.0) alpha_two_OK = true
          if (alpha_one_OK || alpha_two_OK) {
            var alpha = alpha_one
            if (alpha_one_OK && alpha_two_OK) {
              alpha = Math.max(alpha_one, alpha_two)
            } else if (alpha_two_OK) {
              alpha = alpha_two
            }
            val k = ceiling(Math.max(d / alpha, (h + 1) / (2.0 * epsilon)).toFloat)
            if (k > 0) {
              val memory = b * k
              if (memory < best_memory) {
                best_k = k
                best_b = b
                best_h = h
                best_memory = memory
              }
            }
          }
          h += 1
        }
        b += 1
      }
      if (best_b == Long.MAX_VALUE) {
        println("Warning: Computing b and k looks like a lot of work!")
        max_b *= 2
        max_h *= 2
        max_H *= 2
      }
    }
    val result = Array.ofDim[Long](3)
    if (best_b == Long.MAX_VALUE) {
      result(0) = 1
      result(1) = Long.MAX_VALUE
      result(2) = Long.MAX_VALUE
    } else {
      result(0) = best_b
      result(1) = best_k
      result(2) = best_h
    }
    result
  }
}
