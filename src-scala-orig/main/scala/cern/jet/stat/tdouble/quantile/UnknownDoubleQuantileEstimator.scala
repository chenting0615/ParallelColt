package cern.jet.stat.tdouble.quantile

import cern.colt.list.tdouble.DoubleArrayList
import cern.colt.list.tobject.ObjectArrayList
import cern.jet.random.tdouble.engine.DoubleRandomEngine
import cern.jet.random.tdouble.sampling.WeightedDoubleRandomSampler
import cern.jet.stat.Utils
import UnknownDoubleQuantileEstimator._
//remove if not needed
import scala.collection.JavaConversions._

object UnknownDoubleQuantileEstimator {

  /**
   * To do. This could faster be done without sorting (min and second min).
   */
  protected def sortAscendingByLevel(fullBuffers: Array[DoubleBuffer]) {
    new ObjectArrayList(fullBuffers).quickSortFromTo(0, fullBuffers.length - 1, new java.util.Comparator() {

      def compare(o1: AnyRef, o2: AnyRef): Int = {
        val l1 = o1.asInstanceOf[DoubleBuffer].level()
        val l2 = o2.asInstanceOf[DoubleBuffer].level()
        if (l1 < l2) -1 else if (l1 == l2) 0 else +1
      }
    })
  }
}

/**
 * Approximate quantile finding algorithm for unknown <tt>N</tt> requiring only
 * one pass and little main memory; computes quantiles over a sequence of
 * <tt>double</tt> elements. This algorithm requires at most two times the
 * memory of a corresponding approx. quantile finder knowing <tt>N</tt>.
 *
 * <p>
 * Needs as input the following parameters:
 * <p>
 * <dt>1. <tt>quantiles</tt> - the number of quantiles to be computed.
 * <dt>2. <tt>epsilon</tt> - the allowed approximation error on quantiles. The
 * approximation guarantee of this algorithm is explicit.
 *
 * <p>
 * It is also possible to couple the approximation algorithm with random
 * sampling to further reduce memory requirements. With sampling, the
 * approximation guarantees are explicit but probabilistic, i.e. they apply with
 * respect to a (user controlled) confidence parameter "delta".
 *
 * <dt>3. <tt>delta</tt> - the probability allowed that the approximation error
 * fails to be smaller than epsilon. Set <tt>delta</tt> to zero for explicit non
 * probabilistic guarantees.
 *
 * You usually don't instantiate quantile finders by using the constructor.
 * Instead use the factory <tt>QuantileFinderFactor</tt> to do so. It will set
 * up the right parametrization for you.
 *
 * <p>
 * After Gurmeet Singh Manku, Sridhar Rajagopalan and Bruce G. Lindsay, Random
 * Sampling Techniques for Space Efficient Online Computation of Order
 * Statistics of Large Datasets. Accepted for Proc. of the 1999 ACM SIGMOD Int.
 * Conf. on Management of Data, Paper (soon) available <A
 * HREF="http://www-cad.eecs.berkeley.edu/~manku"> here</A>.
 *
 * @author wolfgang.hoschek@cern.ch
 * @version 1.0, 09/24/99
 * @see DoubleQuantileFinderFactory
 * @see KnownApproximateDoubleQuantileFinder
 */
@SerialVersionUID(1L)
class UnknownDoubleQuantileEstimator(b: Int, 
    k: Int, 
    h: Int, 
    protected var precomputeEpsilon: Double, 
    generator: DoubleRandomEngine) extends DoubleQuantileEstimator {

  protected var currentTreeHeight: Int = _

  protected val treeHeightStartingSampling = h

  protected var sampler: WeightedDoubleRandomSampler = new WeightedDoubleRandomSampler(1, generator)

  setUp(b, k)

  this.clear()

  /**
   * Not yet commented.
   */
  protected def buffersToCollapse(): Array[DoubleBuffer] = {
    val fullBuffers = bufferSet._getFullOrPartialBuffers()
    sortAscendingByLevel(fullBuffers)
    val minLevel = fullBuffers(1).level()
    if (fullBuffers(0).level() < minLevel) {
      fullBuffers(0).level(minLevel)
    }
    bufferSet._getFullOrPartialBuffersWithLevel(minLevel)
  }

  /**
   * Removes all elements from the receiver. The receiver will be empty after
   * this call returns, and its memory requirements will be close to zero.
   */
  def clear() {
    synchronized {
      super.clear()
      this.currentTreeHeight = 1
      this.sampler.setWeight(1)
    }
  }

  /**
   * Returns a deep copy of the receiver.
   *
   * @return a deep copy of the receiver.
   */
  def clone(): AnyRef = {
    val copy = super.clone().asInstanceOf[UnknownDoubleQuantileEstimator]
    if (this.sampler != null) copy.sampler = copy.sampler.clone().asInstanceOf[WeightedDoubleRandomSampler]
    copy
  }

  /**
   * Not yet commented.
   */
  protected def newBuffer() {
    currentBufferToFill = bufferSet._getFirstEmptyBuffer()
    if (currentBufferToFill == null) throw new RuntimeException("Oops, no empty buffer.")
    currentBufferToFill.level(currentTreeHeight - 1)
    currentBufferToFill.weight(sampler.getWeight)
  }

  /**
   * Not yet commented.
   */
  protected def postCollapse(toCollapse: Array[DoubleBuffer]) {
    if (toCollapse.length == bufferSet.b()) {
      currentTreeHeight += 1
      if (currentTreeHeight >= treeHeightStartingSampling) {
        sampler.setWeight(sampler.getWeight * 2)
      }
    }
  }

  /**
   * Computes the specified quantile elements over the values previously
   * added.
   *
   * @param phis
   *            the quantiles for which elements are to be computed. Each phi
   *            must be in the interval (0.0,1.0]. <tt>phis</tt> must be
   *            sorted ascending.
   * @return the approximate quantile elements.
   */
  def quantileElements(phis: DoubleArrayList): DoubleArrayList = {
    if (precomputeEpsilon <= 0.0) return super.quantileElements(phis)
    val quantilesToPrecompute = Utils.epsilonCeiling(1.0 / precomputeEpsilon).toInt
    phis = phis.copy()
    val e = precomputeEpsilon
    var index = phis.size
    while (index >= 0) {
      val phi = phis.get(index)
      var i = Math.round(((2.0 * phi / e) - 1.0) / 2.0).toInt
      i = Math.min(quantilesToPrecompute - 1, Math.max(0, i))
      val augmentedPhi = (e / 2.0) * (1 + 2 * i)
      phis.set(index, augmentedPhi)
    }
    super.quantileElements(phis)
  }

  /**
   * Not yet commented.
   */
  protected def sampleNextElement(): Boolean = sampler.sampleNextElement()

  /**
   * Returns a String representation of the receiver.
   */
  override def toString(): String = {
    val buf = new StringBuffer(super.toString)
    buf.setLength(buf.length - 1)
    buf + ", h=" + currentTreeHeight + ", hStartSampling=" + 
      treeHeightStartingSampling + 
      ", precomputeEpsilon=" + 
      precomputeEpsilon + 
      ")"
  }
}
