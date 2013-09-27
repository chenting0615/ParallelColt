package hep.aida.tfloat.bin

import cern.colt.list.tfloat.FloatArrayList
import cern.jet.stat.tfloat.FloatDescriptive
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Static and the same as its superclass, except that it can do more:
 * Additionally computes moments of arbitrary integer order, harmonic mean,
 * geometric mean, etc.
 *
 * Constructors need to be told what functionality is required for the given use
 * case. Only maintains aggregate measures (incrementally) - the added elements
 * themselves are not kept.
 *
 * @author wolfgang.hoschek@cern.ch
 * @version 0.9, 03-Jul-99
 */
@SerialVersionUID(1L)
class MightyStaticFloatBin1D(protected var hasSumOfLogarithms: Boolean, protected var hasSumOfInversions: Boolean, maxOrderForSumOfPowers: Int)
    extends StaticFloatBin1D {

  protected var sumOfLogarithms: Float = 0.0f

  protected var sumOfInversions: Float = 0.0f

  protected var sumOfPowers: Array[Float] = null

  setMaxOrderForSumOfPowers(maxOrderForSumOfPowers)

  this.clear()

  /**
   * Constructs and returns an empty bin with limited functionality but good
   * performance; equivalent to <tt>MightyStaticBin1D(false,false,4)</tt>.
   */
  def this() {
    this(false, false, 4)
  }

  /**
   * Adds the part of the specified list between indexes <tt>from</tt>
   * (inclusive) and <tt>to</tt> (inclusive) to the receiver.
   *
   * @param list
   *            the list of which elements shall be added.
   * @param from
   *            the index of the first element to be added (inclusive).
   * @param to
   *            the index of the last element to be added (inclusive).
   * @throws IndexOutOfBoundsException
   *             if
   *             <tt>list.size()&gt;0 && (from&lt;0 || from&gt;to || to&gt;=list.size())</tt>
   *             .
   */
  def addAllOfFromTo(list: FloatArrayList, from: Int, to: Int) {
    synchronized {
      super.addAllOfFromTo(list, from, to)
      if (this.sumOfPowers != null) {
        FloatDescriptive.incrementalUpdateSumsOfPowers(list, from, to, 3, getMaxOrderForSumOfPowers, 
          this.sumOfPowers)
      }
      if (this.hasSumOfInversions) {
        this.sumOfInversions += FloatDescriptive.sumOfInversions(list, from, to)
      }
      if (this.hasSumOfLogarithms) {
        this.sumOfLogarithms += FloatDescriptive.sumOfLogarithms(list, from, to)
      }
    }
  }

  /**
   * Resets the values of all measures.
   */
  protected def clearAllMeasures() {
    super.clearAllMeasures()
    this.sumOfLogarithms = 0.0f
    this.sumOfInversions = 0.0f
    if (this.sumOfPowers != null) {
      var i = this.sumOfPowers.length
      while (i >= 0) {
        this.sumOfPowers(i) = 0.0f
      }
    }
  }

  /**
   * Returns a deep copy of the receiver.
   *
   * @return a deep copy of the receiver.
   */
  def clone(): AnyRef = {
    synchronized {
      val clone = super.clone().asInstanceOf[MightyStaticFloatBin1D]
      if (this.sumOfPowers != null) clone.sumOfPowers = clone.sumOfPowers.clone()
      clone
    }
  }

  /**
   * Computes the deviations from the receiver's measures to another bin's
   * measures.
   *
   * @param other
   *            the other bin to compare with
   * @return a summary of the deviations.
   */
  def compareWith(other: AbstractFloatBin1D): String = {
    val buf = new StringBuffer(super.compareWith(other))
    if (other.isInstanceOf[MightyStaticFloatBin1D]) {
      val m = other.asInstanceOf[MightyStaticFloatBin1D]
      if (hasSumOfLogarithms() && m.hasSumOfLogarithms()) buf.append("geometric mean: " + relError(geometricMean(), 
        m.geometricMean()) + 
        " %\n")
      if (hasSumOfInversions() && m.hasSumOfInversions()) buf.append("harmonic mean: " + relError(harmonicMean(), 
        m.harmonicMean()) + 
        " %\n")
      if (hasSumOfPowers(3) && m.hasSumOfPowers(3)) buf.append("skew: " + relError(skew(), m.skew()) + " %\n")
      if (hasSumOfPowers(4) && m.hasSumOfPowers(4)) buf.append("kurtosis: " + relError(kurtosis(), m.kurtosis()) + " %\n")
      buf.append("\n")
    }
    buf.toString
  }

  /**
   * Returns the geometric mean, which is
   * <tt>Product( x[i] )<sup>1.0/size()</sup></tt>.
   *
   * This method tries to avoid overflows at the expense of an equivalent but
   * somewhat inefficient definition:
   * <tt>geoMean = exp( Sum( Log(x[i]) ) / size())</tt>. Note that for a
   * geometric mean to be meaningful, the minimum of the data sequence must
   * not be less or equal to zero.
   *
   * @return the geometric mean; <tt>Float.NaN</tt> if
   *         <tt>!hasSumOfLogarithms()</tt>.
   */
  def geometricMean(): Float = {
    synchronized {
      FloatDescriptive.geometricMean(size, sumOfLogarithms())
    }
  }

  /**
   * Returns the maximum order <tt>k</tt> for which sums of powers are
   * retrievable, as specified upon instance construction.
   *
   * @see #hasSumOfPowers(int)
   * @see #sumOfPowers(int)
   */
  def getMaxOrderForSumOfPowers(): Int = {
    synchronized {
      if (this.sumOfPowers == null) return 2
      2 + this.sumOfPowers.length
    }
  }

  /**
   * Returns the minimum order <tt>k</tt> for which sums of powers are
   * retrievable, as specified upon instance construction.
   *
   * @see #hasSumOfPowers(int)
   * @see #sumOfPowers(int)
   */
  def getMinOrderForSumOfPowers(): Int = {
    synchronized {
      var minOrder = 0
      if (hasSumOfInversions()) minOrder = -1
      minOrder
    }
  }

  /**
   * Returns the harmonic mean, which is <tt>size() / Sum( 1/x[i] )</tt>.
   * Remember: If the receiver contains at least one element of <tt>0.0</tt>,
   * the harmonic mean is <tt>0.0</tt>.
   *
   * @return the harmonic mean; <tt>Float.NaN</tt> if
   *         <tt>!hasSumOfInversions()</tt>.
   * @see #hasSumOfInversions()
   */
  def harmonicMean(): Float = {
    synchronized {
      FloatDescriptive.harmonicMean(size, sumOfInversions())
    }
  }

  /**
   * Returns whether <tt>sumOfInversions()</tt> can return meaningful results.
   *
   * @return <tt>false</tt> if the bin was constructed with insufficient
   *         parametrization, <tt>true</tt> otherwise. See the constructors
   *         for proper parametrization.
   */
  def hasSumOfInversions(): Boolean = this.hasSumOfInversions

  /**
   * Tells whether <tt>sumOfLogarithms()</tt> can return meaningful results.
   *
   * @return <tt>false</tt> if the bin was constructed with insufficient
   *         parametrization, <tt>true</tt> otherwise. See the constructors
   *         for proper parametrization.
   */
  def hasSumOfLogarithms(): Boolean = this.hasSumOfLogarithms

  /**
   * Tells whether <tt>sumOfPowers(k)</tt> can return meaningful results.
   * Defined as
   * <tt>hasSumOfPowers(k) <==> getMinOrderForSumOfPowers() <= k && k <= getMaxOrderForSumOfPowers()</tt>
   * . A return value of <tt>true</tt> implies that
   * <tt>hasSumOfPowers(k-1) .. hasSumOfPowers(0)</tt> will also return
   * <tt>true</tt>. See the constructors for proper parametrization.
   * <p>
   * <b>Details</b>: <tt>hasSumOfPowers(0..2)</tt> will always yield
   * <tt>true</tt>. <tt>hasSumOfPowers(-1) <==> hasSumOfInversions()</tt>.
   *
   * @return <tt>false</tt> if the bin was constructed with insufficient
   *         parametrization, <tt>true</tt> otherwise.
   * @see #getMinOrderForSumOfPowers()
   * @see #getMaxOrderForSumOfPowers()
   */
  def hasSumOfPowers(k: Int): Boolean = {
    getMinOrderForSumOfPowers <= k && k <= getMaxOrderForSumOfPowers
  }

  /**
   * Returns the kurtosis (aka excess), which is
   * <tt>-3 + moment(4,mean()) / standardDeviation()<sup>4</sup></tt>.
   *
   * @return the kurtosis; <tt>Float.NaN</tt> if <tt>!hasSumOfPowers(4)</tt>.
   * @see #hasSumOfPowers(int)
   */
  def kurtosis(): Float = {
    synchronized {
      FloatDescriptive.kurtosis(moment(4, mean()), standardDeviation())
    }
  }

  /**
   * Returns the moment of <tt>k</tt>-th order with value <tt>c</tt>, which is
   * <tt>Sum( (x[i]-c)<sup>k</sup> ) / size()</tt>.
   *
   * @param k
   *            the order; must be greater than or equal to zero.
   * @param c
   *            any number.
   * @throws IllegalArgumentException
   *             if <tt>k < 0</tt>.
   * @return <tt>Float.NaN</tt> if <tt>!hasSumOfPower(k)</tt>.
   */
  def moment(k: Int, c: Float): Float = {
    synchronized {
      if (k < 0) throw new IllegalArgumentException("k must be >= 0")
      if (!hasSumOfPowers(k)) return Float.NaN
      val maxOrder = Math.min(k, getMaxOrderForSumOfPowers)
      val sumOfPows = new FloatArrayList(maxOrder + 1)
      sumOfPows.add(size)
      sumOfPows.add(sum())
      sumOfPows.add(sumOfSquares())
      var i = 3
      while (i <= maxOrder) {sumOfPows.add(sumOfPowers(i))i += 1
      }
      FloatDescriptive.moment(k, c, size, sumOfPows.elements())
    }
  }

  /**
   * Returns the product, which is <tt>Prod( x[i] )</tt>. In other words:
   * <tt>x[0]*x[1]*...*x[size()-1]</tt>.
   *
   * @return the product; <tt>Float.NaN</tt> if <tt>!hasSumOfLogarithms()</tt>
   *         .
   * @see #hasSumOfLogarithms()
   */
  def product(): Float = {
    FloatDescriptive.product(size, sumOfLogarithms())
  }

  /**
   * Sets the range of orders in which sums of powers are to be computed. In
   * other words, <tt>sumOfPower(k)</tt> will return <tt>Sum( x[i]^k )</tt> if
   * <tt>min_k <= k <= max_k || 0 <= k <= 2</tt> and throw an exception
   * otherwise.
   *
   * @see #isLegalOrder(int)
   * @see #sumOfPowers(int)
   * @see #getRangeForSumOfPowers()
   */
  protected def setMaxOrderForSumOfPowers(max_k: Int) {
    this.sumOfPowers = if (max_k <= 2) null else Array.ofDim[Float](max_k - 2)
  }

  /**
   * Returns the skew, which is
   * <tt>moment(3,mean()) / standardDeviation()<sup>3</sup></tt>.
   *
   * @return the skew; <tt>Float.NaN</tt> if <tt>!hasSumOfPowers(3)</tt>.
   * @see #hasSumOfPowers(int)
   */
  def skew(): Float = {
    synchronized {
      FloatDescriptive.skew(moment(3, mean()), standardDeviation())
    }
  }

  /**
   * Returns the sum of inversions, which is <tt>Sum( 1 / x[i] )</tt>.
   *
   * @return the sum of inversions; <tt>Float.NaN</tt> if
   *         <tt>!hasSumOfInversions()</tt>.
   * @see #hasSumOfInversions()
   */
  def sumOfInversions(): Float = {
    if (!this.hasSumOfInversions) return Float.NaN
    this.sumOfInversions
  }

  /**
   * Returns the sum of logarithms, which is <tt>Sum( Log(x[i]) )</tt>.
   *
   * @return the sum of logarithms; <tt>Float.NaN</tt> if
   *         <tt>!hasSumOfLogarithms()</tt>.
   * @see #hasSumOfLogarithms()
   */
  def sumOfLogarithms(): Float = {
    synchronized {
      if (!this.hasSumOfLogarithms) return Float.NaN
      this.sumOfLogarithms
    }
  }

  /**
   * Returns the <tt>k-th</tt> order sum of powers, which is
   * <tt>Sum( x[i]<sup>k</sup> )</tt>.
   *
   * @param k
   *            the order of the powers.
   * @return the sum of powers; <tt>Float.NaN</tt> if
   *         <tt>!hasSumOfPowers(k)</tt>.
   * @see #hasSumOfPowers(int)
   */
  def sumOfPowers(k: Int): Float = {
    synchronized {
      if (!hasSumOfPowers(k)) return Float.NaN
      if (k == -1) return sumOfInversions()
      if (k == 0) return size
      if (k == 1) return sum()
      if (k == 2) return sumOfSquares()
      this.sumOfPowers(k - 3)
    }
  }

  /**
   * Returns a String representation of the receiver.
   */
  override def toString(): String = {
    synchronized {
      val buf = new StringBuffer(super.toString)
      if (hasSumOfLogarithms()) {
        buf.append("Geometric mean: " + geometricMean())
        buf.append("\nProduct: " + product() + "\n")
      }
      if (hasSumOfInversions()) {
        buf.append("Harmonic mean: " + harmonicMean())
        buf.append("\nSum of inversions: " + sumOfInversions() + "\n")
      }
      val maxOrder = getMaxOrderForSumOfPowers
      val maxPrintOrder = Math.min(6, maxOrder)
      if (maxOrder > 2) {
        if (maxOrder >= 3) {
          buf.append("Skew: " + skew() + "\n")
        }
        if (maxOrder >= 4) {
          buf.append("Kurtosis: " + kurtosis() + "\n")
        }
        var i = 3
        while (i <= maxPrintOrder) {
          buf.append("Sum of powers(" + i + "): " + sumOfPowers(i) + "\n")
          i += 1
          i += 1
        }
        var k = 0
        while (k <= maxPrintOrder) {
          buf.append("Moment(" + k + ",0): " + moment(k, 0) + "\n")
          k += 1
          k += 1
        }
        var k = 0
        while (k <= maxPrintOrder) {
          buf.append("Moment(" + k + ",mean()): " + moment(k, mean()) + "\n")
          k += 1
          k += 1
        }
      }
      buf.toString
    }
  }

  /**
   * @throws IllegalOperationException
   *             if <tt>! isLegalOrder(k)</tt>.
   */
  protected def xcheckOrder(k: Int) {
  }

  /**
   * Returns whether two bins are equal; They are equal if the other object is
   * of the same class or a subclass of this class and both have the same
   * size, minimum, maximum, sum, sumOfSquares, sumOfInversions and
   * sumOfLogarithms.
   */
  protected def xequals(`object`: AnyRef): Boolean = {
    if (!(`object`.isInstanceOf[MightyStaticFloatBin1D])) return false
    val other = `object`.asInstanceOf[MightyStaticFloatBin1D]
    super == other && sumOfInversions() == other.sumOfInversions() && 
      sumOfLogarithms() == other.sumOfLogarithms()
  }

  /**
   * Tells whether <tt>sumOfPowers(fromK) .. sumOfPowers(toK)</tt> can return
   * meaningful results.
   *
   * @return <tt>false</tt> if the bin was constructed with insufficient
   *         parametrization, <tt>true</tt> otherwise. See the constructors
   *         for proper parametrization.
   * @throws IllegalArgumentException
   *             if <tt>fromK > toK</tt>.
   */
  protected def xhasSumOfPowers(fromK: Int, toK: Int): Boolean = {
    if (fromK > toK) throw new IllegalArgumentException("fromK must be less or equal to toK")
    getMinOrderForSumOfPowers <= fromK && toK <= getMaxOrderForSumOfPowers
  }

  /**
   * Returns
   * <tt>getMinOrderForSumOfPowers() <= k && k <= getMaxOrderForSumOfPowers()</tt>
   * .
   */
  protected def xisLegalOrder(k: Int): Boolean = {
    synchronized {
      getMinOrderForSumOfPowers <= k && k <= getMaxOrderForSumOfPowers
    }
  }
}
