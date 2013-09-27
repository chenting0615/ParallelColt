package cern.jet.stat.tfloat.quantile

import cern.jet.math.tfloat.FloatArithmetic
import cern.jet.random.tfloat.engine.FloatRandomEngine
import FloatQuantileFinderFactory._
//remove if not needed
import scala.collection.JavaConversions._

object FloatQuantileFinderFactory {

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
   * @param returnSamplingRate
   *            output parameter, a <tt>float[1]</tt> where the sampling rate
   *            is to be filled in.
   * @return <tt>long[2]</tt> - <tt>long[0]</tt>=the number of buffers,
   *         <tt>long[1]</tt>=the number of elements per buffer,
   *         <tt>returnSamplingRate[0]</tt>=the required sampling rate.
   */
  def known_N_compute_B_and_K(N: Long, 
      epsilon: Float, 
      delta: Float, 
      quantiles: Int, 
      returnSamplingRate: Array[Float]): Array[Long] = {
    returnSamplingRate(0) = 1.0f
    if (epsilon <= 0.0) {
      val result = Array.ofDim[Long](2)
      result(0) = 1
      result(1) = N
      return result
    }
    if (epsilon >= 1.0 || delta >= 1.0) {
      val result = Array.ofDim[Long](2)
      result(0) = 2
      result(1) = 1
      return result
    }
    if (delta > 0.0) {
      return known_N_compute_B_and_K_slow(N, epsilon, delta, quantiles, returnSamplingRate)
    }
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
    val maxBuffers = 50
    val maxHeight = 50
    val N_float = N
    val c = N_float * epsilon * 2.0f
    val heightMaximums = Array.ofDim[Int](maxBuffers - 1)
    var b = 2
    while (b <= maxBuffers) {
      var h = 3
      while (h <= maxHeight && 
        (h - 2) * (FloatArithmetic.binomial(b + h - 2, h - 1)) - 
        (FloatArithmetic.binomial(b + h - 3, h - 3)) + 
        (FloatArithmetic.binomial(b + h - 3, h - 2)) - 
        c > 
        0.0) {
        h += 1
      }
      while (h <= maxHeight && 
        (h - 2) * (FloatArithmetic.binomial(b + h - 2, h - 1)) - 
        (FloatArithmetic.binomial(b + h - 3, h - 3)) + 
        (FloatArithmetic.binomial(b + h - 3, h - 2)) - 
        c <= 
        0.0) {
        h += 1
      }
      h -= 1
      var hMax: Int = 0
      hMax = if (h >= maxHeight && 
        (h - 2) * (FloatArithmetic.binomial(b + h - 2, h - 1)) - 
        (FloatArithmetic.binomial(b + h - 3, h - 3)) + 
        (FloatArithmetic.binomial(b + h - 3, h - 2)) - 
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
        val value = (FloatArithmetic.binomial(b + h - 2, h - 1))
        val tmpK = (Math.ceil(N_float / value)).toLong
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
      val binomial = FloatArithmetic.binomial(b + h - 2, h - 1)
      val tmp = Math.ceil(N_float / binomial).toLong
      if ((b * tmp < memory) && 
        ((h - 2) * binomial - FloatArithmetic.binomial(b + h - 3, h - 3) + 
        FloatArithmetic.binomial(b + h - 3, h - 2) <= 
        c)) {
        ret_k = tmp
        ret_b = b
        memory = ret_k * b
        sampling_rate = 1.0f
      }
      if (delta > 0.0) {
        val t = (h - 2) * FloatArithmetic.binomial(b + h - 2, h - 1) - 
          FloatArithmetic.binomial(b + h - 3, h - 3) + 
          FloatArithmetic.binomial(b + h - 3, h - 2)
        val u = logarithm / epsilon
        val v = FloatArithmetic.binomial(b + h - 2, h - 1)
        val w = logarithm / (2.0f * epsilon * epsilon)
        val x = (0.5 + 0.5 * Math.sqrt(1.0 + 4.0 * t / u)).toFloat
        val k = Math.ceil(w * x * x / v).toLong
        if (b * k < memory) {
          ret_k = k
          ret_b = b
          memory = b * k
          sampling_rate = N_float * 2.0f * epsilon * epsilon / logarithm
        }
      }
    }
    val result = Array.ofDim[Long](2)
    result(0) = ret_b
    result(1) = ret_k
    returnSamplingRate(0) = sampling_rate
    result
  }

  /**
   * Returns a quantile finder that minimizes the amount of memory needed
   * under the user provided constraints.
   *
   * Many applications don't know in advance over how many elements quantiles
   * are to be computed. However, some of them can give an upper limit, which
   * will assist the factory in choosing quantile finders with minimal memory
   * requirements. For example if you select values from a database and fill
   * them into histograms, then you probably don't know how many values you
   * will fill, but you probably do know that you will fill at most <tt>S</tt>
   * elements, the size of your database.
   *
   * @param known_N
   *            specifies whether the number of elements over which quantiles
   *            are to be computed is known or not.
   * @param N
   *            if <tt>known_N==true</tt>, the number of elements over which
   *            quantiles are to be computed. if <tt>known_N==false</tt>, the
   *            upper limit on the number of elements over which quantiles are
   *            to be computed. If such an upper limit is a-priori unknown,
   *            then set <tt>N = Long.MAX_VALUE</tt>.
   * @param epsilon
   *            the approximation error which is guaranteed not to be exceeded
   *            (e.g. <tt>0.001</tt>) (<tt>0 &lt;= epsilon &lt;= 1</tt>). To
   *            get exact result, set <tt>epsilon=0.0</tt>;
   * @param delta
   *            the probability that the approximation error is more than than
   *            epsilon (e.g. 0.0001) (0 &lt;= delta &lt;= 1). To avoid
   *            probabilistic answers, set <tt>delta=0.0</tt>.
   * @param quantiles
   *            the number of quantiles to be computed (e.g. <tt>100</tt>) (
   *            <tt>quantiles &gt;= 1</tt>). If unknown in advance, set this
   *            number large, e.g. <tt>quantiles &gt;= 10000</tt>.
   * @param generator
   *            a uniform random number generator. Set this parameter to
   *            <tt>null</tt> to use a default generator.
   * @return the quantile finder minimizing memory requirements under the
   *         given constraints.
   */
  def newFloatQuantileFinder(known_N: Boolean, 
      N: Long, 
      epsilon: Float, 
      delta: Float, 
      quantiles: Int, 
      generator: FloatRandomEngine): FloatQuantileFinder = {
    if (epsilon <= 0.0 || N < 1000) return new ExactFloatQuantileFinder()
    if (epsilon > 1) epsilon = 1
    if (delta < 0) delta = 0
    if (delta > 1) delta = 1
    if (quantiles < 1) quantiles = 1
    if (quantiles > N) N = quantiles
    val finder: KnownFloatQuantileEstimator = null
    if (known_N) {
      val samplingRate = Array.ofDim[Float](1)
      val resultKnown = known_N_compute_B_and_K(N, epsilon, delta, quantiles, samplingRate)
      val b = resultKnown(0)
      val k = resultKnown(1)
      if (b == 1) return new ExactFloatQuantileFinder()
      new KnownFloatQuantileEstimator(b.toInt, k.toInt, N, samplingRate(0), generator)
    } else {
      val resultUnknown = unknown_N_compute_B_and_K(epsilon, delta, quantiles)
      val b1 = resultUnknown(0)
      val k1 = resultUnknown(1)
      val h1 = resultUnknown(2)
      var preComputeEpsilon = -1.0f
      if (resultUnknown(3) == 1) preComputeEpsilon = epsilon
      if (true) {
        if (b1 == 1) return new ExactFloatQuantileFinder()
        return new UnknownFloatQuantileEstimator(b1.toInt, k1.toInt, h1.toInt, preComputeEpsilon, generator)
      }
      val samplingRate = Array.ofDim[Float](1)
      val resultKnown = known_N_compute_B_and_K(N, epsilon, 0, quantiles, samplingRate)
      val b2 = resultKnown(0)
      val k2 = resultKnown(1)
      if (b2 * k2 < b1 * k1) {
        if (b2 == 1) return new ExactFloatQuantileFinder()
        return new KnownFloatQuantileEstimator(b2.toInt, k2.toInt, N, samplingRate(0), generator)
      }
      if (b1 == 1) return new ExactFloatQuantileFinder()
      new UnknownFloatQuantileEstimator(b1.toInt, k1.toInt, h1.toInt, preComputeEpsilon, generator)
    }
  }

  /**
   * Convenience method that computes phi's for equi-depth histograms. This is
   * simply a list of numbers with <tt>i / (float)quantiles</tt> for
   * <tt>i={1,2,...,quantiles-1}</tt>.
   *
   * @return the equi-depth phi's
   */
  def newEquiDepthPhis(quantiles: Int): cern.colt.list.tfloat.FloatArrayList = {
    val phis = new cern.colt.list.tfloat.FloatArrayList(quantiles - 1)
    var i = 1
    while (i <= quantiles - 1) {phis.add(i / quantiles.toFloat)i += 1
    }
    phis
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
   * @return <tt>long[4]</tt> - <tt>long[0]</tt>=the number of buffers,
   *         <tt>long[1]</tt>=the number of elements per buffer,
   *         <tt>long[2]</tt>=the tree height where sampling shall start,
   *         <tt>long[3]==1</tt> if precomputing is better, otherwise 0;
   */
  def unknown_N_compute_B_and_K(epsilon: Float, delta: Float, quantiles: Int): Array[Long] = {
    unknown_N_compute_B_and_K_raw(epsilon, delta, quantiles)
  }

  /**
   * Computes the number of buffers and number of values per buffer such that
   * quantiles can be determined with an approximation error no more than
   * epsilon with a certain probability. <b>You never need to call this
   * method.</b> It is only for curious users wanting to gain some insight
   * into the workings of the algorithms.
   *
   * @param epsilon
   *            the approximation error which is guaranteed not to be exceeded
   *            (e.g. <tt>0.001</tt>) (<tt>0 &lt;= epsilon &lt;= 1</tt>). To
   *            get exact result, set <tt>epsilon=0.0</tt>;
   * @param delta
   *            the probability that the approximation error is more than than
   *            epsilon (e.g. <tt>0.0001</tt>) (<tt>0 &lt;= delta &lt;= 1</tt>
   *            ). To get exact results, set <tt>delta=0.0</tt>.
   * @param quantiles
   *            the number of quantiles to be computed (e.g. <tt>100</tt>) (
   *            <tt>quantiles &gt;= 1</tt>). If unknown in advance, set this
   *            number large, e.g. <tt>quantiles &gt;= 10000</tt>.
   * @return <tt>long[4]</tt> - <tt>long[0]</tt>=the number of buffers,
   *         <tt>long[1]</tt>=the number of elements per buffer,
   *         <tt>long[2]</tt>=the tree height where sampling shall start,
   *         <tt>long[3]==1</tt> if precomputing is better, otherwise 0;
   */
  protected def unknown_N_compute_B_and_K_raw(epsilon: Float, delta: Float, quantiles: Int): Array[Long] = {
    if (epsilon <= 0.0) {
      val result = Array.ofDim[Long](4)
      result(0) = 1
      result(1) = Long.MAX_VALUE
      result(2) = Long.MAX_VALUE
      result(3) = 0
      return result
    }
    if (epsilon >= 1.0 || delta >= 1.0) {
      val result = Array.ofDim[Long](4)
      result(0) = 2
      result(1) = 1
      result(2) = 3
      result(3) = 0
      return result
    }
    if (delta <= 0.0) {
      val result = Array.ofDim[Long](4)
      result(0) = 1
      result(1) = Long.MAX_VALUE
      result(2) = Long.MAX_VALUE
      result(3) = 0
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
          val Ld = FloatArithmetic.binomial(b + h - 2, h - 1)
          val Ls = FloatArithmetic.binomial(b + h - 3, h - 1)
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
            val k = Math.ceil(Math.max(d / alpha, (h + 1) / (2.0 * epsilon))).toLong
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
    val result = Array.ofDim[Long](4)
    result(3) = 0
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

/**
 * Factory constructing exact and approximate quantile finders for both known
 * and unknown <tt>N</tt>. Also see {@link hep.aida.tfloat.bin.QuantileFloatBin1D},
 * demonstrating how this package can be used.
 *
 * The approx. algorithms compute approximate quantiles of large data sequences
 * in a single pass. The approximation guarantees are explicit, and apply for
 * arbitrary value distributions and arrival distributions of the data sequence.
 * The main memory requirements are smaller than for any other known technique
 * by an order of magnitude.
 *
 * <p>
 * The approx. algorithms are primarily intended to help applications scale.
 * When faced with a large data sequences, traditional methods either need very
 * large memories or time consuming disk based sorting. In constrast, the
 * approx. algorithms can deal with > 10^10 values without disk based sorting.
 *
 * <p>
 * All classes can be seen from various angles, for example as
 * <dt>1. Algorithm to compute quantiles.
 * <dt>2. 1-dim-equi-depth histogram.
 * <dt>3. 1-dim-histogram arbitrarily rebinnable in real-time.
 * <dt>4. A space efficient MultiSet data structure using lossy compression.
 * <dt>5. A space efficient value preserving bin of a 2-dim or d-dim histogram.
 * <dt>(All subject to an accuracy specified by the user.)
 *
 * <p>
 * Use methods <tt>newXXX(...)</tt> to get new instances of one of the
 * following quantile finders.
 *
 * <p>
 * <b>1. Exact quantile finding algorithm for known and unknown <tt>N</tt>
 * requiring large main memory.</b>
 * </p>
 * The folkore algorithm: Keeps all elements in main memory, sorts the list,
 * then picks the quantiles.
 *
 *
 *
 *
 * <p>
 * <p>
 * <b>2. Approximate quantile finding algorithm for known <tt>N</tt> requiring
 * only one pass and little main memory.</b>
 * </p>
 *
 * <p>
 * Needs as input the following parameters:
 * <p>
 * <dt>1. <tt>N</tt> - the number of values of the data sequence over which
 * quantiles are to be determined.
 * <dt>2. <tt>quantiles</tt> - the number of quantiles to be computed. If
 * unknown in advance, set this number large, e.g.
 * <tt>quantiles &gt;= 10000</tt>.
 * <dt>3. <tt>epsilon</tt> - the allowed approximation error on quantiles.
 * The approximation guarantee of this algorithm is explicit.
 *
 * <p>
 * It is also possible to couple the approximation algorithm with random
 * sampling to further reduce memory requirements. With sampling, the
 * approximation guarantees are explicit but probabilistic, i.e. they apply with
 * respect to a (user controlled) confidence parameter "delta".
 *
 * <dt>4. <tt>delta</tt> - the probability allowed that the approximation
 * error fails to be smaller than epsilon. Set <tt>delta</tt> to zero for
 * explicit non probabilistic guarantees.
 *
 * <p>
 * After Gurmeet Singh Manku, Sridhar Rajagopalan and Bruce G. Lindsay,
 * Approximate Medians and other Quantiles in One Pass and with Limited Memory,
 * Proc. of the 1998 ACM SIGMOD Int. Conf. on Management of Data, Paper
 * available <A
 * HREF="http://www-cad.eecs.berkeley.edu/~manku/papers/quantiles.ps.gz"> here</A>.
 *
 *
 *
 *
 * <p>
 * <p>
 * <b>3. Approximate quantile finding algorithm for unknown <tt>N</tt>
 * requiring only one pass and little main memory.</b>
 * </p>
 * This algorithm requires at most two times the memory of a corresponding
 * approx. quantile finder knowing <tt>N</tt>.
 *
 * <p>
 * Needs as input the following parameters:
 * <p>
 * <dt>2. <tt>quantiles</tt> - the number of quantiles to be computed. If
 * unknown in advance, set this number large, e.g. <tt>quantiles &gt;= 1000</tt>.
 * <dt>2. <tt>epsilon</tt> - the allowed approximation error on quantiles.
 * The approximation guarantee of this algorithm is explicit.
 *
 * <p>
 * It is also possible to couple the approximation algorithm with random
 * sampling to further reduce memory requirements. With sampling, the
 * approximation guarantees are explicit but probabilistic, i.e. they apply with
 * respect to a (user controlled) confidence parameter "delta".
 *
 * <dt>3. <tt>delta</tt> - the probability allowed that the approximation
 * error fails to be smaller than epsilon. Set <tt>delta</tt> to zero for
 * explicit non probabilistic guarantees.
 *
 * <p>
 * After Gurmeet Singh Manku, Sridhar Rajagopalan and Bruce G. Lindsay, Random
 * Sampling Techniques for Space Efficient Online Computation of Order
 * Statistics of Large Datasets. Proc. of the 1999 ACM SIGMOD Int. Conf. on
 * Management of Data, Paper available <A
 * HREF="http://www-cad.eecs.berkeley.edu/~manku/papers/unknown.ps.gz"> here</A>.
 *
 * <p>
 * <b>Example usage:</b>
 *
 * <pre>
 * _TODO_
 * </pre>
 *
 * <p>
 *
 *
 * @author wolfgang.hoschek@cern.ch
 * @version 1.0, 09/24/99
 * @see KnownFloatQuantileEstimator
 * @see UnknownFloatQuantileEstimator
 */
class FloatQuantileFinderFactory protected () extends AnyRef
