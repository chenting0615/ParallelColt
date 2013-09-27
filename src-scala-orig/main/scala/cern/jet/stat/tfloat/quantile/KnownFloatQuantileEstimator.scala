package cern.jet.stat.tfloat.quantile

import cern.colt.list.tfloat.FloatArrayList
import cern.jet.math.tfloat.FloatArithmetic
import cern.jet.random.tfloat.engine.FloatRandomEngine
import cern.jet.random.tfloat.sampling.FloatRandomSamplingAssistant
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Approximate quantile finding algorithm for known <tt>N</tt> requiring only
 * one pass and little main memory; computes quantiles over a sequence of
 * <tt>float</tt> elements.
 *
 * <p>
 * Needs as input the following parameters:
 * <p>
 * <dt>1. <tt>N</tt> - the number of values of the data sequence over which
 * quantiles are to be determined.
 * <dt>2. <tt>quantiles</tt> - the number of quantiles to be computed.
 * <dt>3. <tt>epsilon</tt> - the allowed approximation error on quantiles. The
 * approximation guarantee of this algorithm is explicit.
 *
 * <p>
 * It is also possible to couple the approximation algorithm with random
 * sampling to further reduce memory requirements. With sampling, the
 * approximation guarantees are explicit but probabilistic, i.e. they apply with
 * respect to a (user controlled) confidence parameter "delta".
 *
 * <dt>4. <tt>delta</tt> - the probability allowed that the approximation error
 * fails to be smaller than epsilon. Set <tt>delta</tt> to zero for explicit non
 * probabilistic guarantees.
 *
 * You usually don't instantiate quantile finders by using the constructor.
 * Instead use the factory <tt>QuantileFinderFactor</tt> to do so. It will set
 * up the right parametrization for you.
 *
 * <p>
 * After Gurmeet Singh Manku, Sridhar Rajagopalan and Bruce G. Lindsay,
 * Approximate Medians and other Quantiles in One Pass and with Limited Memory,
 * Proc. of the 1998 ACM SIGMOD Int. Conf. on Management of Data, Paper
 * available <A HREF="http://www-cad.eecs.berkeley.edu/~manku"> here</A>.
 *
 * @author wolfgang.hoschek@cern.ch
 * @version 1.0, 09/24/99
 * @see FloatQuantileFinderFactory
 * @see UnknownApproximateFloatQuantileFinder
 */
@SerialVersionUID(1L)
class KnownFloatQuantileEstimator(b: Int, 
    k: Int, 
    protected var N: Long, 
    protected var samplingRate: Float, 
    generator: FloatRandomEngine) extends FloatQuantileEstimator {

  protected var beta: Float = _

  protected var weHadMoreThanOneEmptyBuffer: Boolean = _

  protected var samplingAssistant: FloatRandomSamplingAssistant = if (this.samplingRate <= 1.0) null else new FloatRandomSamplingAssistant(FloatArithmetic.floor(N / samplingRate), 
    N, generator)

  setUp(b, k)

  this.clear()

  /**
   * @param infinities
   *            the number of infinities to fill.
   * @param buffer
   *            the buffer into which the infinities shall be filled.
   */
  protected def addInfinities(missingInfinities: Int, buffer: FloatBuffer) {
    val oldAssistant = this.samplingAssistant
    this.samplingAssistant = null
    var even = true
    for (i <- 0 until missingInfinities) {
      if (even) buffer.values.add(Float.MAX_VALUE) else buffer.values.add(-Float.MAX_VALUE)
      even = !even
    }
    this.samplingAssistant = oldAssistant
  }

  /**
   * Not yet commented.
   */
  protected def buffersToCollapse(): Array[FloatBuffer] = {
    val minLevel = bufferSet._getMinLevelOfFullOrPartialBuffers()
    bufferSet._getFullOrPartialBuffersWithLevel(minLevel)
  }

  /**
   * Removes all elements from the receiver. The receiver will be empty after
   * this call returns, and its memory requirements will be close to zero.
   */
  def clear() {
    super.clear()
    this.beta = 1.0f
    this.weHadMoreThanOneEmptyBuffer = false
    val assist = this.samplingAssistant
    if (assist != null) {
      this.samplingAssistant = new FloatRandomSamplingAssistant(FloatArithmetic.floor(N / samplingRate), 
        N, assist.getRandomGenerator)
    }
  }

  /**
   * Returns a deep copy of the receiver.
   *
   * @return a deep copy of the receiver.
   */
  def clone(): AnyRef = {
    val copy = super.clone().asInstanceOf[KnownFloatQuantileEstimator]
    if (this.samplingAssistant != null) copy.samplingAssistant = copy.samplingAssistant.clone().asInstanceOf[FloatRandomSamplingAssistant]
    copy
  }

  /**
   * Not yet commented.
   */
  protected def newBuffer() {
    val numberOfEmptyBuffers = this.bufferSet._getNumberOfEmptyBuffers()
    if (numberOfEmptyBuffers == 0) throw new RuntimeException("Oops, no empty buffer.")
    this.currentBufferToFill = this.bufferSet._getFirstEmptyBuffer()
    if (numberOfEmptyBuffers == 1 && !this.weHadMoreThanOneEmptyBuffer) {
      this.currentBufferToFill.level(this.bufferSet._getMinLevelOfFullOrPartialBuffers())
    } else {
      this.weHadMoreThanOneEmptyBuffer = true
      this.currentBufferToFill.level(0)
    }
    this.currentBufferToFill.weight(1)
  }

  /**
   * Not yet commented.
   */
  protected def postCollapse(toCollapse: Array[FloatBuffer]) {
    this.weHadMoreThanOneEmptyBuffer = false
  }

  /**
   */
  protected def preProcessPhis(phis: FloatArrayList): FloatArrayList = {
    if (beta > 1.0) {
      phis = phis.copy()
      var i = phis.size
      while (i >= 0) {
        phis.set(i, (2 * phis.get(i) + beta - 1) / (2 * beta))
      }
    }
    phis
  }

  /**
   * Computes the specified quantile elements over the values previously
   * added.
   *
   * @param phis
   *            the quantiles for which elements are to be computed. Each phi
   *            must be in the interval [0.0,1.0]. <tt>phis</tt> must be
   *            sorted ascending.
   * @return the approximate quantile elements.
   */
  def quantileElements(phis: FloatArrayList): FloatArrayList = {
    val partial = this.bufferSet._getPartialBuffer()
    var missingValues = 0
    if (partial != null) {
      missingValues = bufferSet.k() - partial.size
      if (missingValues <= 0) throw new RuntimeException("Oops! illegal missing values.")
      this.addInfinities(missingValues, partial)
      this.beta = (this.totalElementsFilled + missingValues) / this.totalElementsFilled.toFloat
    } else {
      this.beta = 1.0f
    }
    val quantileElements = super.quantileElements(phis)
    if (partial != null) removeInfinitiesFrom(missingValues, partial)
    quantileElements
  }

  /**
   * Reading off quantiles requires to fill some +infinity, -infinity values
   * to make a partial buffer become full.
   *
   * This method removes the infinities which were previously temporarily
   * added to a partial buffer. Removing them is necessary if we want to
   * continue filling more elements. Precondition: the buffer is sorted
   * ascending.
   *
   * @param infinities
   *            the number of infinities previously filled.
   * @param buffer
   *            the buffer into which the infinities were filled.
   */
  protected def removeInfinitiesFrom(infinities: Int, buffer: FloatBuffer) {
    var plusInf = 0
    var minusInf = 0
    var even = true
    for (i <- 0 until infinities) {
      if (even) plusInf += 1 else minusInf += 1
      even = !even
    }
    buffer.values.removeFromTo(buffer.size - plusInf, buffer.size - 1)
    buffer.values.removeFromTo(0, minusInf - 1)
  }

  /**
   * Not yet commented.
   */
  protected def sampleNextElement(): Boolean = {
    if (samplingAssistant == null) return true
    samplingAssistant.sampleNextElement()
  }
}
