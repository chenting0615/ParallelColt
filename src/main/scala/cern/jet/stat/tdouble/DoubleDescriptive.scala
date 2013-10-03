package cern.jet.stat.tdouble

import java.util
import cern.colt.list.ArrayTypes.{IntArrayList, DoubleArrayList}

object DoubleDescriptive {

  /**
   * Returns the auto-correlation of a data sequence.
   */
  def autoCorrelation(data: DoubleArrayList,
      lag: Int,
      mean: Double,
      variance: Double): Double = {
    val N = data.size
    if (lag >= N) throw new IllegalArgumentException("Lag is too large")
    val elements = data.elements()
    var run = 0.0
    for (i <- lag until N) run += (elements(i) - mean) * (elements(i - lag) - mean)
    (run / (N - lag)) / variance
  }

  /**
   * Checks if the given range is within the contained array's bounds.
   *
   * @throws IndexOutOfBoundsException
   *             if
   *             <tt>to!=from-1 || from&lt;0 || from&gt;to || to&gt;=size()</tt>
   *             .
   */
  protected def checkRangeFromTo(from: Int, to: Int, theSize: Int) {
    if (to == from - 1) return
    if (from < 0 || from > to || to >= theSize) throw new IndexOutOfBoundsException("from: " + from + ", to: " + to + ", size=" + theSize)
  }

  /**
   * Returns the correlation of two data sequences. That is
   * <tt>covariance(data1,data2)/(standardDev1*standardDev2)</tt>.
   */
  def correlation(data1: DoubleArrayList,
      standardDev1: Double,
      data2: DoubleArrayList,
      standardDev2: Double): Double = {
    covariance(data1, data2) / (standardDev1 * standardDev2)
  }

  /**
   * Returns the covariance of two data sequences, which is
   * <tt>cov(x,y) = (1/(size()-1)) * Sum((x[i]-mean(x)) * (y[i]-mean(y)))</tt>
   * . See the <A
   * HREF="http://www.cquest.utoronto.ca/geog/ggr270y/notes/not05efg.html">
   * math definition</A>.
   */
  def covariance(data1: DoubleArrayList, data2: DoubleArrayList): Double = {
    val size = data1.size
    if (size != data2.size || size == 0) throw new IllegalArgumentException()
    val elements1 = data1.elements()
    val elements2 = data2.elements()
    var sumx = elements1(0)
    var sumy = elements2(0)
    var Sxy = 0.0
    for (i <- 1 until size) {
      val x = elements1(i)
      val y = elements2(i)
      sumx += x
      Sxy += (x - sumx / (i + 1)) * (y - sumy / i)
      sumy += y
    }
    Sxy / (size - 1)
  }

  /**
   * Durbin-Watson computation.
   */
  def durbinWatson(data: DoubleArrayList): Double = {
    val size = data.size
    if (size < 2) throw new IllegalArgumentException("data sequence must contain at least two values.")
    val elements = data.elements()
    var run = 0.0
    var run_sq = elements(0) * elements(0)
    for (i <- 1 until size) {
      val x = elements(i) - elements(i - 1)
      run += x * x
      run_sq += elements(i) * elements(i)
    }
    run / run_sq
  }

  /**
   * Computes the frequency (number of occurances, count) of each distinct
   * value in the given sorted data. After this call returns both
   * <tt>distinctValues</tt> and <tt>frequencies</tt> have a new size (which
   * is equal for both), which is the number of distinct values in the sorted
   * data.
   * <p>
   * Distinct values are filled into <tt>distinctValues</tt>, starting at
   * index 0. The frequency of each distinct value is filled into
   * <tt>frequencies</tt>, starting at index 0. As a result, the smallest
   * distinct value (and its frequency) can be found at index 0, the second
   * smallest distinct value (and its frequency) at index 1, ..., the largest
   * distinct value (and its frequency) at index
   * <tt>distinctValues.size()-1</tt>.
   *
   * <b>Example:</b> <br>
   * <tt>elements = (5,6,6,7,8,8) --> distinctValues = (5,6,7,8), frequencies = (1,2,1,2)</tt>
   *
   * @param sortedData
   *            the data; must be sorted ascending.
   * @param distinctValues
   *            a list to be filled with the distinct values; can have any
   *            size.
   * @param frequencies
   *            a list to be filled with the frequencies; can have any size;
   *            set this parameter to <tt>null</tt> to ignore it.
   */
  def frequencies(sortedData: DoubleArrayList, distinctValues: DoubleArrayList, frequencies: IntArrayList) {
    distinctValues.clear()
    if (frequencies != null) frequencies.clear()
    val sortedElements = sortedData.elements()
    val size = sortedData.size
    var i = 0
    while (i < size) {
      val element = sortedElements(i)
      val cursor = i
      while (i < size && sortedElements(i) == element) i += 1
      val runLength = i - cursor
      distinctValues.add(element)
      if (frequencies != null) frequencies.add(runLength)
      i += 1
    }
  }

  /**
   * Returns the geometric mean of a data sequence. Note that for a geometric
   * mean to be meaningful, the minimum of the data sequence must not be less
   * or equal to zero. <br>
   * The geometric mean is given by <tt>pow( Product( data[i] ), 1/size)</tt>
   * which is equivalent to <tt>Math.exp( Sum( Log(data[i]) ) / size)</tt>.
   */
  def geometricMean(size: Int, sumOfLogarithms: Double): Double = Math.exp(sumOfLogarithms / size)

  /**
   * Returns the geometric mean of a data sequence. Note that for a geometric
   * mean to be meaningful, the minimum of the data sequence must not be less
   * or equal to zero. <br>
   * The geometric mean is given by
   * <tt>pow( Product( data[i] ), 1/data.size())</tt>. This method tries to
   * avoid overflows at the expense of an equivalent but somewhat slow
   * definition: <tt>geo = Math.exp( Sum( Log(data[i]) ) / data.size())</tt>.
   */
  def geometricMean(data: DoubleArrayList): Double = {
    geometricMean(data.size, sumOfLogarithms(data, 0, data.size - 1))
  }

  /**
   * Returns the harmonic mean of a data sequence.
   *
   * @param size
   *            the number of elements in the data sequence.
   * @param sumOfInversions
   *            <tt>Sum( 1.0 / data[i])</tt>.
   */
  def harmonicMean(size: Int, sumOfInversions: Double): Double = size / sumOfInversions

  /**
   * Incrementally maintains and updates minimum, maximum, sum and sum of
   * squares of a data sequence.
   *
   * Assume we have already recorded some data sequence elements and know
   * their minimum, maximum, sum and sum of squares. Assume further, we are to
   * record some more elements and to derive updated values of minimum,
   * maximum, sum and sum of squares.
   * <p>
   * This method computes those updated values without needing to know the
   * already recorded elements. Returns the updated values filled into the
   * <tt>inOut</tt> array.
   *
   * This is interesting for interactive online monitoring and/or applications
   * that cannot keep the entire huge data sequence in memory.
   * <p>
   * <br>
   * Definition of sumOfSquares:
   * <tt>sumOfSquares(n) = Sum ( data[i] * data[i] )</tt>.
   *
   *
   * @param data
   *            the additional elements to be incorporated into min, max, etc.
   * @param from_p
   *            the index of the first element within <tt>data</tt> to
   *            consider.
   * @param to
   *            the index of the last element within <tt>data</tt> to
   *            consider. The method incorporates elements
   *            <tt>data[from], ..., data[to]</tt>.
   * @param inOut
   *            the old values in the following format:
   *            <ul>
   *            <li><tt>inOut[0]</tt> is the old minimum. <li><tt>inOut[1]
   *            </tt> is the old maximum. <li><tt>inOut[2]</tt> is the old
   *            sum. <li><tt>inOut[3]</tt> is the old sum of squares.
   *            </ul>
   *            If no data sequence elements have so far been recorded set the
   *            values as follows
   *            <ul>
   *            <li><tt>inOut[0] = Double.POSITIVE_INFINITY</tt> as the old
   *            minimum. <li><tt>inOut[1] = Double.NEGATIVE_INFINITY</tt> as
   *            the old maximum. <li><tt>inOut[2] = 0.0</tt> as the old sum.
   *            <li><tt>inOut[3] = 0.0</tt> as the old sum of squares.
   *            </ul>
   *
   */
  def incrementalUpdate(data: DoubleArrayList,
      from_p: Int,
      to: Int,
      inOut: Array[Double]) {
    var from = from_p
    checkRangeFromTo(from, to, data.size)
    var min = inOut(0)
    var max = inOut(1)
    var sum = inOut(2)
    var sumSquares = inOut(3)
    val elements = data.elements()
    while (from <= to) {
      val element = elements(from)
      sum += element
      sumSquares += element * element
      if (element < min) min = element
      if (element > max) max = element
      from += 1
    }
    inOut(0) = min
    inOut(1) = max
    inOut(2) = sum
    inOut(3) = sumSquares
  }

  /**
   * Incrementally maintains and updates various sums of powers of the form
   * <tt>Sum(data[i]<sup>k</sup>)</tt>.
   *
   * Assume we have already recorded some data sequence elements
   * <tt>data[i]</tt> and know the values of
   * <tt>Sum(data[i]<sup>from</sup>), Sum(data[i]<sup>from+1</sup>), ..., Sum(data[i]<sup>to</sup>)</tt>
   * . Assume further, we are to record some more elements and to derive
   * updated values of these sums.
   * <p>
   * This method computes those updated values without needing to know the
   * already recorded elements. Returns the updated values filled into the
   * <tt>sumOfPowers</tt> array. This is interesting for interactive online
   * monitoring and/or applications that cannot keep the entire huge data
   * sequence in memory. For example, the incremental computation of moments
   * is based upon such sums of powers:
   * <p>
   * The moment of <tt>k</tt>-th order with constant <tt>c</tt> of a data
   * sequence, is given by
   * <tt>Sum( (data[i]-c)<sup>k</sup> ) / data.size()</tt>. It can
   * incrementally be computed by using the equivalent formula
   * <p>
   * <tt>moment(k,c) = m(k,c) / data.size()</tt> where <br>
   * <tt>m(k,c) = Sum( -1<sup>i</sup> * b(k,i) * c<sup>i</sup> * sumOfPowers(k-i))</tt>
   * for <tt>i = 0 .. k</tt> and <br>
   * <tt>b(k,i) = </tt>
   * cern.jet.math.tdouble.DoubleArithmetic.binomial(long,long)
   * binomial(k,i) and <br>
   * <tt>sumOfPowers(k) = Sum( data[i]<sup>k</sup> )</tt>.
   * <p>
   *
   * @param data
   *            the additional elements to be incorporated into min, max, etc.
   * @param from
   *            the index of the first element within <tt>data</tt> to
   *            consider.
   * @param to
   *            the index of the last element within <tt>data</tt> to
   *            consider. The method incorporates elements
   *            <tt>data[from], ..., data[to]</tt>.
   *
   * @param sumOfPowers
   *            the old values of the sums in the following format:
   *            <ul>
   *            <li><tt>sumOfPowers[0]</tt> is the old <tt>
   *            Sum(data[i]<sup>fromSumIndex</sup>)</tt>. <li><tt>
   *            sumOfPowers[1]</tt> is the old <tt>
   *            Sum(data[i]<sup>fromSumIndex+1</sup>)</tt>. <li>... <li><tt>
   *            sumOfPowers[toSumIndex-fromSumIndex]</tt> is the old <tt>
   *            Sum(data[i]<sup>toSumIndex</sup>)</tt>.
   *            </ul>
   *            If no data sequence elements have so far been recorded set all
   *            old values of the sums to <tt>0.0</tt>.
   *
   *
   */
  def incrementalUpdateSumsOfPowers(data: DoubleArrayList,
      from: Int,
      to: Int,
      fromSumIndex: Int,
      toSumIndex: Int,
      sumOfPowers: Array[Double]) {
    val size = data.size
    val lastIndex = toSumIndex - fromSumIndex
    if (from > size || lastIndex + 1 > sumOfPowers.length) throw new IllegalArgumentException()
    if (fromSumIndex == 1) {
      if (toSumIndex == 2) {
        val elements = data.elements()
        var sum = sumOfPowers(0)
        var sumSquares = sumOfPowers(1)
        for(i <- from until to+1) {
          val element = elements(i)
          sum += element
          sumSquares += element * element
        }
        sumOfPowers(0) += sum
        sumOfPowers(1) += sumSquares
        return
      }
      else if (toSumIndex == 3) {
        val elements = data.elements()
        var sum = sumOfPowers(0)
        var sumSquares = sumOfPowers(1)
        var sum_xxx = sumOfPowers(2)
        for(i <- from until to+1) {
          val element = elements(i)
          sum += element
          sumSquares += element * element
          sum_xxx += element * element * element
        }
        sumOfPowers(0) += sum
        sumOfPowers(1) += sumSquares
        sumOfPowers(2) += sum_xxx
        return
      }
      else if (toSumIndex == 4) {
        val elements = data.elements()
        var sum = sumOfPowers(0)
        var sumSquares = sumOfPowers(1)
        var sum_xxx = sumOfPowers(2)
        var sum_xxxx = sumOfPowers(3)
        for(i <- from until to+1) {
          val element = elements(i)
          sum += element
          sumSquares += element * element
          sum_xxx += element * element * element
          sum_xxxx += element * element * element * element
        }
        sumOfPowers(0) += sum
        sumOfPowers(1) += sumSquares
        sumOfPowers(2) += sum_xxx
        sumOfPowers(3) += sum_xxxx
        return
      }
    }
    if (fromSumIndex == toSumIndex || (fromSumIndex >= -1 && toSumIndex <= 5)) {
      var i = fromSumIndex
      while (i <= toSumIndex) {
        sumOfPowers(i - fromSumIndex) += sumOfPowerDeviations(data, i, 0.0, from, to)
        i += 1
      }
      return
    }
    val elements = data.elements()
    var i = from
    while (i <= to) {
      val element = elements(i)
      var pow = Math.pow(element, fromSumIndex)
      var j = 0
      var m = lastIndex
      while (m >= 0) {
        sumOfPowers(j) += pow
        j += 1
        pow *= element
        m -= 1
      }
      sumOfPowers(j) += pow
      i += 1
    }
  }

  /**
   * Incrementally maintains and updates sum and sum of squares of a
   * <i>weighted</i> data sequence.
   *
   * Assume we have already recorded some data sequence elements and know
   * their sum and sum of squares. Assume further, we are to record some more
   * elements and to derive updated values of sum and sum of squares.
   * <p>
   * This method computes those updated values without needing to know the
   * already recorded elements. Returns the updated values filled into the
   * <tt>inOut</tt> array. This is interesting for interactive online
   * monitoring and/or applications that cannot keep the entire huge data
   * sequence in memory.
   * <p>
   * <br>
   * Definition of sum: <tt>sum = Sum ( data[i] * weights[i] )</tt>. <br>
   * Definition of sumOfSquares:
   * <tt>sumOfSquares = Sum ( data[i] * data[i] * weights[i])</tt>.
   *
   *
   * @param data
   *            the additional elements to be incorporated into min, max, etc.
   * @param weights
   *            the weight of each element within <tt>data</tt>.
   * @param from
   *            the index of the first element within <tt>data</tt> (and
   *            <tt>weights</tt>) to consider.
   * @param to
   *            the index of the last element within <tt>data</tt> (and
   *            <tt>weights</tt>) to consider. The method incorporates
   *            elements <tt>data[from], ..., data[to]</tt>.
   * @param inOut
   *            the old values in the following format:
   *            <ul>
   *            <li><tt>inOut[0]</tt> is the old sum. <li><tt>inOut[1]</tt> is
   *            the old sum of squares.
   *            </ul>
   *            If no data sequence elements have so far been recorded set the
   *            values as follows
   *            <ul>
   *            <li><tt>inOut[0] = 0.0</tt> as the old sum. <li><tt>inOut[1] =
   *            0.0</tt> as the old sum of squares.
   *            </ul>
   *
   */
  def incrementalWeightedUpdate(data: DoubleArrayList,
      weights: DoubleArrayList,
      from: Int,
      to: Int,
      inOut: Array[Double]) {
    val dataSize = data.size
    checkRangeFromTo(from, to, dataSize)
    if (dataSize != weights.size)
      throw new IllegalArgumentException("from=" + from + ", to=" + to + ", data.size()=" + dataSize + ", weights.size()=" + weights.size)
    var sum = inOut(0)
    var sumOfSquares = inOut(1)
    val elements = data.elements()
    val w = weights.elements()
    for(i <- from until to+1) {
      val element = elements(i)
      val weight = w(i)
      val prod = element * weight
      sum += prod
      sumOfSquares += element * prod
    }
    inOut(0) = sum
    inOut(1) = sumOfSquares
  }

  /**
   * Returns the kurtosis (aka excess) of a data sequence.
   *
   * @param moment4
   *            the fourth central moment, which is
   *            <tt>moment(data,4,mean)</tt>.
   * @param standardDeviation
   *            the standardDeviation.
   */
  def kurtosis(moment4: Double, standardDeviation: Double): Double = {
    -3 +
      moment4 /
      (standardDeviation * standardDeviation * standardDeviation *
      standardDeviation)
  }

  /**
   * Returns the kurtosis (aka excess) of a data sequence, which is
   * <tt>-3 + moment(data,4,mean) / standardDeviation<sup>4</sup></tt>.
   */
  def kurtosis(data: DoubleArrayList, mean: Double, standardDeviation: Double): Double = {
    kurtosis(moment(data, 4, mean), standardDeviation)
  }

  /**
   * Returns the lag-1 autocorrelation of a dataset; Note that this method has
   * semantics different from <tt>autoCorrelation(..., 1)</tt>;
   */
  def lag1(data: DoubleArrayList, mean: Double): Double = {
    val size = data.size
    val elements = data.elements()
    var r1 = 0.0
    var q = 0.0
    var v = (elements(0) - mean) * (elements(0) - mean)
    for (i <- 1 until size) {
      val delta0 = elements(i - 1) - mean
      val delta1 = elements(i) - mean
      q += (delta0 * delta1 - q) / (i + 1)
      v += (delta1 * delta1 - v) / (i + 1)
    }
    r1 = q / v
    r1
  }

  /**
   * Returns the largest member of a data sequence.
   */
  def max(data: DoubleArrayList): Double = {
    if (data.size == 0) throw new IllegalArgumentException()
    val elements = data.elements()
    var max = Double.NegativeInfinity
    for(i <- 0 until data.size) {
      if (elements(i) > max) max = elements(i)
    }
    max
  }

  /**
   * Returns the arithmetic mean of a data sequence; That is
   * <tt>Sum( data[i] ) / data.size()</tt>.
   */
  def mean(data: DoubleArrayList): Double = sum(data) / data.size

  /**
   * Returns the mean deviation of a dataset. That is
   * <tt>Sum (Math.abs(data[i]-mean)) / data.size())</tt>.
   */
  def meanDeviation(data: DoubleArrayList, mean: Double): Double = {
    val elements = data.elements()
    var sum = 0.0
    for(i <- 0 until data.size) sum += Math.abs(elements(i) - mean)
    sum / data.size
  }

  /**
   * Returns the median of a sorted data sequence.
   *
   * @param sortedData
   *            the data sequence; <b>must be sorted ascending</b>.
   */
  def median(sortedData: DoubleArrayList): Double = quantile(sortedData, 0.5)

  /**
   * Returns the smallest member of a data sequence.
   */
  def min(data: DoubleArrayList): Double = {
    val size = data.size
    if (size == 0) throw new IllegalArgumentException()
    val elements = data.elements()
    var min = elements(size - 1)
    var i = size - 1
    while (i >= 0) {
      if (elements(i) < min) min = elements(i)
    }
    min
  }

  /**
   * Returns the moment of <tt>k</tt>-th order with constant <tt>c</tt> of a
   * data sequence, which is
   * <tt>Sum( (data[i]-c)<sup>k</sup> ) / data.size()</tt>.
   *
   * @param sumOfPowers
   *            <tt>sumOfPowers[m] == Sum( data[i]<sup>m</sup>) )</tt> for
   *            <tt>m = 0,1,..,k</tt> as returned by method
   *            incrementalUpdateSumsOfPowers(DoubleArrayList,int,int,int,int,double[])
   *            . In particular there must hold
   *            <tt>sumOfPowers.length == k+1</tt>.
   * @param size
   *            the number of elements of the data sequence.
   */
  def moment(k: Int,
      c: Double,
      size: Int,
      sumOfPowers: Array[Double]): Double = {
    var sum = 0.0
    var sign = 1
    var i = 0
    while (i <= k) {
      var y: Double = 0.0
      y = if (i == 0) 1 else if (i == 1) c else if (i == 2) c * c else if (i == 3) c * c * c else Math.pow(c,
        i)
      sum += sign *
        cern.jet.math.tdouble.DoubleArithmetic.binomial(k, i) *
        y *
        sumOfPowers(k - i)
      sign = -sign
      i += 1
    }
    sum / size
  }

  /**
   * Returns the moment of <tt>k</tt>-th order with constant <tt>c</tt> of a
   * data sequence, which is
   * <tt>Sum( (data[i]-c)<sup>k</sup> ) / data.size()</tt>.
   */
  def moment(data: DoubleArrayList, k: Int, c: Double): Double = {
    sumOfPowerDeviations(data, k, c) / data.size
  }

  /**
   * Returns the pooled mean of two data sequences. That is
   * <tt>(size1 * mean1 + size2 * mean2) / (size1 + size2)</tt>.
   *
   * @param size1
   *            the number of elements in data sequence 1.
   * @param mean1
   *            the mean of data sequence 1.
   * @param size2
   *            the number of elements in data sequence 2.
   * @param mean2
   *            the mean of data sequence 2.
   */
  def pooledMean(size1: Int,
      mean1: Double,
      size2: Int,
      mean2: Double): Double = {
    (size1 * mean1 + size2 * mean2) / (size1 + size2)
  }

  /**
   * Returns the pooled variance of two data sequences. That is
   * <tt>(size1 * variance1 + size2 * variance2) / (size1 + size2)</tt>;
   *
   * @param size1
   *            the number of elements in data sequence 1.
   * @param variance1
   *            the variance of data sequence 1.
   * @param size2
   *            the number of elements in data sequence 2.
   * @param variance2
   *            the variance of data sequence 2.
   */
  def pooledVariance(size1: Int,
      variance1: Double,
      size2: Int,
      variance2: Double): Double = {
    (size1 * variance1 + size2 * variance2) / (size1 + size2)
  }

  /**
   * Returns the product, which is <tt>Prod( data[i] )</tt>. In other words:
   * <tt>data[0]*data[1]*...*data[data.size()-1]</tt>. This method uses the
   * equivalent definition:
   * <tt>prod = pow( exp( Sum( Log(x[i]) ) / size(), size())</tt>.
   */
  def product(size: Int, sumOfLogarithms: Double): Double = {
    Math.pow(Math.exp(sumOfLogarithms / size), size)
  }

  /**
   * Returns the product of a data sequence, which is <tt>Prod( data[i] )</tt>
   * . In other words: <tt>data[0]*data[1]*...*data[data.size()-1]</tt>. Note
   * that you may easily get numeric overflows.
   */
  def product(data: DoubleArrayList): Double = {
    val elements = data.elements()
    var product = 1.0
   for(i <- 0 until data.size) product *= elements(i)
    product
  }

  /**
   * Returns the <tt>phi-</tt>quantile; that is, an element <tt>elem</tt> for
   * which holds that <tt>phi</tt> percent of data elements are less than
   * <tt>elem</tt>. The quantile need not necessarily be contained in the data
   * sequence, it can be a linear interpolation.
   *
   * @param sortedData
   *            the data sequence; <b>must be sorted ascending</b>.
   * @param phi
   *            the percentage; must satisfy <tt>0 &lt;= phi &lt;= 1</tt>.
   */
  def quantile(sortedData: DoubleArrayList, phi: Double): Double = {
    val sortedElements = sortedData.elements()
    val n = sortedData.size
    val index = phi * (n - 1)
    val lhs = index.toInt
    val delta = index - lhs
    var result: Double = 0.0
    if (n == 0) return 0.0
    result = if (lhs == n - 1) sortedElements(lhs) else (1 - delta) * sortedElements(lhs) + delta * sortedElements(lhs + 1)
    result
  }

  /**
   * Returns how many percent of the elements contained in the receiver are
   * <tt>&lt;= element</tt>. Does linear interpolation if the element is not
   * contained but lies in between two contained elements.
   *
   * @param sortedList
   *            the list to be searched (must be sorted ascending).
   * @param element
   *            the element to search for.
   * @return the percentage <tt>phi</tt> of elements <tt>&lt;= element</tt> (
   *         <tt>0.0 &lt;= phi &lt;= 1.0)</tt>.
   */
  def quantileInverse(sortedList: DoubleArrayList, element: Double): Double = {
    rankInterpolated(sortedList, element) / sortedList.size
  }

  /**
   * Returns the quantiles of the specified percentages. The quantiles need
   * not necessarily be contained in the data sequence, it can be a linear
   * interpolation.
   *
   * @param sortedData
   *            the data sequence; <b>must be sorted ascending</b>.
   * @param percentages
   *            the percentages for which quantiles are to be computed. Each
   *            percentage must be in the interval <tt>[0.0,1.0]</tt>.
   * @return the quantiles.
   */
  def quantiles(sortedData: DoubleArrayList, percentages: DoubleArrayList): DoubleArrayList = {
    val s = percentages.size
    val quantiles = new DoubleArrayList(s)
    for (i <- 0 until s) {
      quantiles.add(quantile(sortedData, percentages.get(i)))
    }
    quantiles
  }

  /**
   * Returns the linearly interpolated number of elements in a list less or
   * equal to a given element. The rank is the number of elements <= element.
   * Ranks are of the form <tt>{0, 1, 2,..., sortedList.size()}</tt>. If no
   * element is <= element, then the rank is zero. If the element lies in
   * between two contained elements, then linear interpolation is used and a
   * non integer value is returned.
   *
   * @param sortedList
   *            the list to be searched (must be sorted ascending).
   * @param element
   *            the element to search for.
   * @return the rank of the element.
   */
  def rankInterpolated(sortedList: DoubleArrayList, element: Double): Double = {
    val values = sortedList.elements()
    val index = util.Arrays.binarySearch(values, element)
    if (index >= 0) {
      var to = index + 1
      val s = sortedList.size
      while (to < s && sortedList.get(to) == element) to += 1
      return to
    }
    val insertionPoint = -index - 1
    if (insertionPoint == 0 || insertionPoint == sortedList.size) return insertionPoint
    val from = sortedList.get(insertionPoint - 1)
    val to = sortedList.get(insertionPoint)
    val delta = (element - from) / (to - from)
    insertionPoint + delta
  }

  /**
   * Returns the RMS (Root-Mean-Square) of a data sequence. That is
   * <tt>Math.sqrt(Sum( data[i]*data[i] ) / data.size())</tt>. The RMS of data
   * sequence is the square-root of the mean of the squares of the elements in
   * the data sequence. It is a measure of the average "size" of the elements
   * of a data sequence.
   *
   * @param sumOfSquares
   *            <tt>sumOfSquares(data) == Sum( data[i]*data[i] )</tt> of the
   *            data sequence.
   * @param size
   *            the number of elements in the data sequence.
   */
  def rms(size: Int, sumOfSquares: Double): Double = Math.sqrt(sumOfSquares / size)

  /**
   * Returns the sample kurtosis (aka excess) of a data sequence.
   *
   * Ref: R.R. Sokal, F.J. Rohlf, Biometry: the principles and practice of
   * statistics in biological research (W.H. Freeman and Company, New York,
   * 1998, 3rd edition) p. 114-115.
   *
   * @param size
   *            the number of elements of the data sequence.
   * @param moment4
   *            the fourth central moment, which is
   *            <tt>moment(data,4,mean)</tt>.
   * @param sampleVariance
   *            the <b>sample variance</b>.
   */
  def sampleKurtosis(size: Int, moment4: Double, sampleVariance: Double): Double = {
    val n = size
    val s2 = sampleVariance
    val m4 = moment4 * n
    m4 * n * (n + 1) / ((n - 1) * (n - 2) * (n - 3) * s2 * s2) -
      3.0 * (n - 1) * (n - 1) / ((n - 2) * (n - 3))
  }

  /**
   * Returns the sample kurtosis (aka excess) of a data sequence.
   */
  def sampleKurtosis(data: DoubleArrayList, mean: Double, sampleVariance: Double): Double = {
    sampleKurtosis(data.size, moment(data, 4, mean), sampleVariance)
  }

  /**
   * Return the standard error of the sample kurtosis.
   *
   * Ref: R.R. Sokal, F.J. Rohlf, Biometry: the principles and practice of
   * statistics in biological research (W.H. Freeman and Company, New York,
   * 1998, 3rd edition) p. 138.
   *
   * @param size
   *            the number of elements of the data sequence.
   */
  def sampleKurtosisStandardError(size: Int): Double = {
    val n = size
    Math.sqrt(24.0 * n * (n - 1) * (n - 1) / ((n - 3) * (n - 2) * (n + 3) * (n + 5)))
  }

  /**
   * Returns the sample skew of a data sequence.
   *
   * Ref: R.R. Sokal, F.J. Rohlf, Biometry: the principles and practice of
   * statistics in biological research (W.H. Freeman and Company, New York,
   * 1998, 3rd edition) p. 114-115.
   *
   * @param size
   *            the number of elements of the data sequence.
   * @param moment3
   *            the third central moment, which is
   *            <tt>moment(data,3,mean)</tt>.
   * @param sampleVariance
   *            the <b>sample variance</b>.
   */
  def sampleSkew(size: Int, moment3: Double, sampleVariance: Double): Double = {
    val n = size
    val s = Math.sqrt(sampleVariance)
    val m3 = moment3 * n
    n * m3 / ((n - 1) * (n - 2) * s * s * s)
  }

  /**
   * Returns the sample skew of a data sequence.
   */
  def sampleSkew(data: DoubleArrayList, mean: Double, sampleVariance: Double): Double = {
    sampleSkew(data.size, moment(data, 3, mean), sampleVariance)
  }

  /**
   * Return the standard error of the sample skew.
   *
   * Ref: R.R. Sokal, F.J. Rohlf, Biometry: the principles and practice of
   * statistics in biological research (W.H. Freeman and Company, New York,
   * 1998, 3rd edition) p. 138.
   *
   * @param size
   *            the number of elements of the data sequence.
   */
  def sampleSkewStandardError(size: Int): Double = {
    val n = size
    Math.sqrt(6.0 * n * (n - 1) / ((n - 2) * (n + 1) * (n + 3)))
  }

  /**
   * Returns the sample standard deviation.
   *
   * Ref: R.R. Sokal, F.J. Rohlf, Biometry: the principles and practice of
   * statistics in biological research (W.H. Freeman and Company, New York,
   * 1998, 3rd edition) p. 53.
   *
   * @param size
   *            the number of elements of the data sequence.
   * @param sampleVariance
   *            the <b>sample variance</b>.
   */
  def sampleStandardDeviation(size: Int, sampleVariance: Double): Double = {
    var s: Double = 0.0
    var Cn: Double = 0.0
    val n = size
    s = Math.sqrt(sampleVariance)
    Cn = if (n > 30) 1 + 1.0 / (4 * (n - 1)) else Math.sqrt((n - 1) * 0.5) * Gamma.gamma((n - 1) * 0.5) /
      Gamma.gamma(n * 0.5)
    Cn * s
  }

  /**
   * Returns the sample variance of a data sequence. That is
   * <tt>(sumOfSquares - mean*sum) / (size - 1)</tt> with
   * <tt>mean = sum/size</tt>.
   *
   * @param size
   *            the number of elements of the data sequence.
   * @param sum
   *            <tt>== Sum( data[i] )</tt>.
   * @param sumOfSquares
   *            <tt>== Sum( data[i]*data[i] )</tt>.
   */
  def sampleVariance(size: Int, sum: Double, sumOfSquares: Double): Double = {
    val mean = sum / size
    (sumOfSquares - mean * sum) / (size - 1)
  }

  /**
   * Returns the sample variance of a data sequence. That is
   * <tt>Sum ( (data[i]-mean)**2 ) / (data.size()-1)</tt>.
   */
  def sampleVariance(data: DoubleArrayList, mean: Double): Double = {
    val elements = data.elements()
    val size = data.size
    var sum = 0.0
    for(i <- 0 until size) {
      val delta = elements(i) - mean
      sum += delta * delta
    }
    sum / (size - 1)
  }

  /**
   * Returns the sample weighted variance of a data sequence. That is
   *
   * <tt>(sumOfSquaredProducts  -  sumOfProducts * sumOfProducts / sumOfWeights) / (sumOfWeights - 1)</tt>
   * .
   *
   * @param sumOfWeights
   *            <tt>== Sum( weights[i] )</tt>.
   * @param sumOfProducts
   *            <tt>== Sum( data[i] * weights[i] )</tt>.
   * @param sumOfSquaredProducts
   *            <tt>== Sum( data[i] * data[i] * weights[i] )</tt>.
   */
  def sampleWeightedVariance(sumOfWeights: Double, sumOfProducts: Double, sumOfSquaredProducts: Double): Double = {
    (sumOfSquaredProducts - sumOfProducts * sumOfProducts / sumOfWeights) /
      (sumOfWeights - 1)
  }

  /**
   * Returns the skew of a data sequence.
   *
   * @param moment3
   *            the third central moment, which is
   *            <tt>moment(data,3,mean)</tt>.
   * @param standardDeviation
   *            the standardDeviation.
   */
  def skew(moment3: Double, standardDeviation: Double): Double = {
    moment3 /
      (standardDeviation * standardDeviation * standardDeviation)
  }

  /**
   * Returns the skew of a data sequence, which is
   * <tt>moment(data,3,mean) / standardDeviation<sup>3</sup></tt>.
   */
  def skew(data: DoubleArrayList, mean: Double, standardDeviation: Double): Double = {
    skew(moment(data, 3, mean), standardDeviation)
  }

  def addAllFromTo(dest: DoubleArrayList, src: DoubleArrayList, from: Int, to: Int) {
    for(idx <- from until to) {
      dest.add(src.get(idx))
    }
  }

  /**
   * Splits (partitions) a list into sublists such that each sublist contains
   * the elements with a given range. <tt>splitters=(a,b,c,...,y,z)</tt>
   * defines the ranges <tt>[-inf,a), [a,b), [b,c), ..., [y,z), [z,inf]</tt>.
   * <p>
   * <b>Examples:</b><br>
   * <ul>
   * <tt>data = (1,2,3,4,5,8,8,8,10,11)</tt>. <br>
   * <tt>splitters=(2,8)</tt> yields 3 bins:
   * <tt>(1), (2,3,4,5) (8,8,8,10,11)</tt>. <br>
   * <tt>splitters=()</tt> yields 1 bin: <tt>(1,2,3,4,5,8,8,8,10,11)</tt>. <br>
   * <tt>splitters=(-5)</tt> yields 2 bins:
   * <tt>(), (1,2,3,4,5,8,8,8,10,11)</tt>. <br>
   * <tt>splitters=(100)</tt> yields 2 bins:
   * <tt>(1,2,3,4,5,8,8,8,10,11), ()</tt>.
   * </ul>
   *
   * @param sortedList
   *            the list to be partitioned (must be sorted ascending).
   * @param splitters
   *            the points at which the list shall be partitioned (must be
   *            sorted ascending).
   * @return the sublists (an array with
   *         <tt>length == splitters.size() + 1</tt>. Each sublist is returned
   *         sorted ascending.
   */
  def split(sortedList: DoubleArrayList, splitters: DoubleArrayList): Array[DoubleArrayList] = {
    sortedList.trimToSize()
    val values = sortedList.elements()
    val noOfBins = splitters.size + 1
    val bins = Array.ofDim[DoubleArrayList](noOfBins)
    for( i <- 0 until noOfBins) bins(i) = new DoubleArrayList()
    val listSize = sortedList.size
    var nextStart = 0
    var i = 0
    while (nextStart < listSize && i < noOfBins - 1) {
      val splitValue = splitters.get(i)
      var index = util.Arrays.binarySearch(values, splitValue)
      if (index < 0) {
        val insertionPosition = -index - 1
        addAllFromTo(bins(i), sortedList, nextStart, insertionPosition - 1)
        nextStart = insertionPosition
      } else {
        do {
          index -= 1
        } while (index >= 0 && sortedList.get(index) == splitValue)
        addAllFromTo(bins(i), sortedList, nextStart, index)
        nextStart = index + 1
      }
      i += 1
    }
    addAllFromTo(bins(noOfBins - 1), sortedList, nextStart, sortedList.size - 1)
    bins
  }

  /**
   * Returns the standard deviation from a variance.
   */
  def standardDeviation(variance: Double): Double = Math.sqrt(variance)

  /**
   * Returns the standard error of a data sequence. That is
   * <tt>Math.sqrt(variance/size)</tt>.
   *
   * @param size
   *            the number of elements in the data sequence.
   * @param variance
   *            the variance of the data sequence.
   */
  def standardError(size: Int, variance: Double): Double = Math.sqrt(variance / size)

  /**
   * Modifies a data sequence to be standardized. Changes each element
   * <tt>data[i]</tt> as follows:
   * <tt>data[i] = (data[i]-mean)/standardDeviation</tt>.
   */
  def standardize(data: DoubleArrayList, mean: Double, standardDeviation: Double) {
    val elements = data.elements()
    for(i <- 0 until data.size) elements(i) = (elements(i) - mean) / standardDeviation
  }

  /**
   * Returns the sum of a data sequence. That is <tt>Sum( data[i] )</tt>.
   */
  def sum(data: DoubleArrayList): Double = sumOfPowerDeviations(data, 1, 0.0)

  /**
   * Returns the sum of inversions of a data sequence, which is
   * <tt>Sum( 1.0 / data[i])</tt>.
   *
   * @param data
   *            the data sequence.
   * @param from
   *            the index of the first data element (inclusive).
   * @param to
   *            the index of the last data element (inclusive).
   */
  def sumOfInversions(data: DoubleArrayList, from: Int, to: Int): Double = {
    sumOfPowerDeviations(data, -1, 0.0, from, to)
  }

  /**
   * Returns the sum of logarithms of a data sequence, which is
   * <tt>Sum( Log(data[i])</tt>.
   *
   * @param data
   *            the data sequence.
   * @param from
   *            the index of the first data element (inclusive).
   * @param to
   *            the index of the last data element (inclusive).
   */
  def sumOfLogarithms(data: DoubleArrayList, from: Int, to: Int): Double = {
    val elements = data.elements()
    var logsum = 0.0
    for(i <- 0 until to+1) logsum += Math.log(elements(i))
    logsum
  }

  /**
   * Returns <tt>Sum( (data[i]-c)<sup>k</sup> )</tt>; optimized for common
   * parameters like <tt>c == 0.0</tt> and/or <tt>k == -2 .. 4</tt>.
   */
  def sumOfPowerDeviations(data: DoubleArrayList, k: Int, c: Double): Double = {
    sumOfPowerDeviations(data, k, c, 0, data.size - 1)
  }

  /**
   * Returns <tt>Sum( (data[i]-c)<sup>k</sup> )</tt> for all
   * <tt>i = from .. to</tt>; optimized for common parameters like
   * <tt>c == 0.0</tt> and/or <tt>k == -2 .. 5</tt>.
   */
  def sumOfPowerDeviations(data: DoubleArrayList,
      k: Int,
      c: Double,
      from: Int,
      to: Int): Double = {
    val elements = data.elements()
    var sum = 0.0
    k match {
      case -2 => {
        if (c == 0.0) {
          for(i <- from until to+1) {
            val v = elements(i)
            sum += 1 / (v * v)
          }
        }
         else {
          for(i <- from until to+1) {
            val v = elements(i) - c
            sum += 1 / (v * v)
          }
        }
      }
      case -1 => {
        if (c == 0.0) {
          for(i <- from until to+1)
            sum += 1 / elements(i)
        }
        else {
          for(i <- from until to+1)
            sum += 1 / (elements(i) - c)
        }
      }
      case 0 => {
        sum += to - from + 1
      }
      case 1 => {
        if (c == 0.0) {
          for(i <- from until to+1) {
            sum += elements(i)
          }
        }
        else {
          for(i <- from until to+1) {
            sum += elements(i) - c
          }
        }
      }
      case 2 => {
        if (c == 0.0) {
          for(i <- from until to+1) {
            val v = elements(i)
            sum += v * v
          }
        }
        else {
          for(i <- from until to+1) {
            val v = elements(i) - c
            sum += v * v
          }
        }
      }
      case 3 => {
        if (c == 0.0) {
          for(i <- from until to+1) {
            val v = elements(i)
            sum += v * v * v
          }
        }
        else {
          for(i <- from until to+1) {
            val v = elements(i) - c
            sum += v * v * v
          }
        }
      }
      case 4 => {
        if (c == 0.0) {
          for(i <- from until to+1) {
            val v = elements(i)
            sum += v * v * v * v
          }
        }
        else {
          for(i <- from until to+1) {
            val v = elements(i) - c
            sum += v * v * v * v
          }
        }
      }
      case 5 => {
        if (c == 0.0) {
          for(i <- from until to+1) {
            val v = elements(i)
            sum += v * v * v * v * v
          }
        }
        else {
          for(i <- from until to+1) {
            val v = elements(i) - c
            sum += v * v * v * v * v
          }
        }
      }
      case _ => {
        for(i <- from until to+1)
          sum += Math.pow(elements(i) - c, k)
      }
    }
    sum
  }

  /**
   * Returns the sum of powers of a data sequence, which is
   * <tt>Sum ( data[i]<sup>k</sup> )</tt>.
   */
  def sumOfPowers(data: DoubleArrayList, k: Int): Double = sumOfPowerDeviations(data, k, 0)

  /**
   * Returns the sum of squared mean deviation of of a data sequence. That is
   * <tt>variance * (size-1) == Sum( (data[i] - mean)**2 )</tt>.
   *
   * @param size
   *            the number of elements of the data sequence.
   * @param variance
   *            the variance of the data sequence.
   */
  def sumOfSquaredDeviations(size: Int, variance: Double): Double = variance * (size - 1)

  /**
   * Returns the sum of squares of a data sequence. That is
   * <tt>Sum ( data[i]*data[i] )</tt>.
   */
  def sumOfSquares(data: DoubleArrayList): Double = sumOfPowerDeviations(data, 2, 0.0)

  /**
   * Returns the trimmed mean of a sorted data sequence.
   *
   * @param sortedData
   *            the data sequence; <b>must be sorted ascending</b>.
   * @param mean_p
   *            the mean of the (full) sorted data sequence.
   * @param left
   *            the number of leading elements to trim.
   * @param right
   *            the number of trailing elements to trim.
   */
  def trimmedMean(sortedData: DoubleArrayList,
      mean_p: Double,
      left: Int,
      right: Int): Double = {
    var mean = mean_p
    val N = sortedData.size
    if (N == 0) throw new IllegalArgumentException("Empty data.")
    if (left + right >= N) throw new IllegalArgumentException("Not enough data.")
    val sortedElements = sortedData.elements()
    val N0 = N
    for (i <- 0 until left) mean += (mean - sortedElements(i)) / N
    for (i <- 0 until right) mean += (mean - sortedElements(N0 - 1 - i)) / N
    mean
  }

  /**
   * Returns the variance from a standard deviation.
   */
  def variance(standardDeviation: Double): Double = standardDeviation * standardDeviation

  /**
   * Returns the variance of a data sequence. That is
   * <tt>(sumOfSquares - mean*sum) / size</tt> with <tt>mean = sum/size</tt>.
   *
   * @param size
   *            the number of elements of the data sequence.
   * @param sum
   *            <tt>== Sum( data[i] )</tt>.
   * @param sumOfSquares
   *            <tt>== Sum( data[i]*data[i] )</tt>.
   */
  def variance(size: Int, sum: Double, sumOfSquares: Double): Double = {
    val mean = sum / size
    (sumOfSquares - mean * sum) / size
  }

  /**
   * Returns the weighted mean of a data sequence. That is
   * <tt> Sum (data[i] * weights[i]) / Sum ( weights[i] )</tt>.
   */
  def weightedMean(data: DoubleArrayList, weights: DoubleArrayList): Double = {
    val size = data.size
    if (size != weights.size || size == 0) throw new IllegalArgumentException()
    val elements = data.elements()
    val theWeights = weights.elements()
    var sum = 0.0
    var weightsSum = 0.0
    for(i <- 0 until size) {
      val w = theWeights(i)
      sum += elements(i) * w
      weightsSum += w
    }
    sum / weightsSum
  }

  /**
   * Returns the weighted RMS (Root-Mean-Square) of a data sequence. That is
   * <tt>Sum( data[i] * data[i] * weights[i]) / Sum( data[i] * weights[i] )</tt>
   * , or in other words <tt>sumOfProducts / sumOfSquaredProducts</tt>.
   *
   * @param sumOfProducts
   *            <tt>== Sum( data[i] * weights[i] )</tt>.
   * @param sumOfSquaredProducts
   *            <tt>== Sum( data[i] * data[i] * weights[i] )</tt>.
   */
  def weightedRMS(sumOfProducts: Double, sumOfSquaredProducts: Double): Double = sumOfProducts / sumOfSquaredProducts

  /**
   * Returns the winsorized mean of a sorted data sequence.
   *
   * @param sortedData
   *            the data sequence; <b>must be sorted ascending</b>.
   * @param mean_p
   *            the mean of the (full) sorted data sequence.
   * @param left
   *            the number of leading elements to trim.
   * @param right
   *            the number of trailing elements to trim.
   */
  def winsorizedMean(sortedData: DoubleArrayList,
      mean_p: Double,
      left: Int,
      right: Int): Double = {
    var mean = mean_p
    val N = sortedData.size
    if (N == 0) throw new IllegalArgumentException("Empty data.")
    if (left + right >= N) throw new IllegalArgumentException("Not enough data.")
    val sortedElements = sortedData.elements()
    val leftElement = sortedElements(left)
    for (i <- 0 until left) mean += (leftElement - sortedElements(i)) / N
    val rightElement = sortedElements(N - 1 - right)
    for (i <- 0 until right) mean += (rightElement - sortedElements(N - 1 - i)) / N
    mean
  }
}

/**
 * Basic descriptive statistics.
 *
 * @author peter.gedeck@pharma.Novartis.com
 * @author wolfgang.hoschek@cern.ch
 * @version 0.91, 08-Dec-99
 */
class DoubleDescriptive protected () extends AnyRef
