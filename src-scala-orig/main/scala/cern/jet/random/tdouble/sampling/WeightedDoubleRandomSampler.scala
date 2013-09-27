package cern.jet.random.tdouble.sampling

import cern.jet.random.tdouble.{AbstractDoubleDistribution, DoubleUniform}
import cern.jet.random.tdouble.engine.DoubleRandomEngine

object WeightedDoubleRandomSampler {

  /**
   * Not yet commented.
   */
  def test(weight: Int, size: Int) {
    val sampler = new WeightedDoubleRandomSampler()
    sampler.setWeight(weight)
    val sample = new cern.colt.list.tint.IntArrayList()
    for (i <- 0 until size if sampler.sampleNextElement()) sample.add(i)
    println("Sample = " + sample)
  }
}

/**
 * Conveniently computes a stable subsequence of elements from a given input
 * sequence; Picks (samples) exactly one random element from successive blocks
 * of <tt>weight</tt> input elements each. For example, if weight==2 (a block is
 * 2 elements), and the input is 5*2=10 elements long, then picks 5 random
 * elements from the 10 elements such that one element is randomly picked from
 * the first block, one element from the second block, ..., one element from the
 * last block. weight == 1.0 --> all elements are picked (sampled). weight ==
 * 10.0 --> Picks one random element from successive blocks of 10 elements each.
 * Etc. The subsequence is guaranteed to be <i>stable</i>, i.e. elements never
 * change position relative to each other.
 *
 * @author wolfgang.hoschek@cern.ch
 * @version 1.0, 02/05/99
 */
@SerialVersionUID(1L)
class WeightedDoubleRandomSampler(protected var weight: Int, var randomGenerator: DoubleRandomEngine)
    extends cern.colt.PersistentObject {

  val UNDEFINED = -1

  protected var skip: Int = _

  protected var nextTriggerPos: Int = _

  protected var nextSkip: Int = _

  protected var generator: DoubleUniform = new DoubleUniform(randomGenerator)

  if (randomGenerator == null) randomGenerator = AbstractDoubleDistribution.makeDefaultGenerator()

  setWeight(weight)

  /**
   * Calls <tt>BlockedRandomSampler(1,null)</tt>.
   */
  def this() {
    this(1, null)
  }

  /**
   * Returns a deep copy of the receiver.
   */
  override def clone(): AnyRef = {
    val copy = super.clone().asInstanceOf[WeightedDoubleRandomSampler]
    copy.generator = this.generator.clone().asInstanceOf[DoubleUniform]
    copy
  }

  def getWeight: Int = this.weight

  /**
   * Chooses exactly one random element from successive blocks of
   * <tt>weight</tt> input elements each. For example, if weight==2, and the
   * input is 5*2=10 elements long, then chooses 5 random elements from the 10
   * elements such that one is chosen from the first block, one from the
   * second, ..., one from the last block.
   *
   * @return <tt>true</tt> if the next element shall be sampled (picked),
   *         <tt>false</tt> otherwise.
   */
  def sampleNextElement(): Boolean = {
    if (skip > 0) {
      skip -= 1
      return false
    }
    if (nextTriggerPos == UNDEFINED) {
      nextTriggerPos = if (weight == 1) 0 else generator.nextIntFromTo(0, weight - 1)
      nextSkip = weight - 1 - nextTriggerPos
    }
    if (nextTriggerPos > 0) {
      nextTriggerPos -= 1
      return false
    }
    nextTriggerPos = UNDEFINED
    skip = nextSkip
    true
  }

  /**
   * Not yet commented.
   *
   * @param weight
   *            int
   */
  def setWeight(weight: Int) {
    if (weight < 1) throw new IllegalArgumentException("bad weight")
    this.weight = weight
    this.skip = 0
    this.nextTriggerPos = UNDEFINED
    this.nextSkip = 0
  }
}
