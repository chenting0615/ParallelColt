package cern.jet.random.tdouble.sampling

import cern.jet.random.tdouble.engine.DoubleRandomEngine

object DoubleRandomSamplingAssistant {

  val MAX_BUFFER_SIZE = 200

  /**
   * Tests random sampling.
   */
  def main(args: Array[String]) {
    val n = args(0).toInt
    val N = args(1).toInt
    testArraySampling(n, N)
  }

  /**
   * Just shows how this class can be used; samples n elements from and int[]
   * array.
   */
  def sampleArray(n: Int, elements: Array[Int]): Array[Int] = {
    val assistant = new DoubleRandomSamplingAssistant(n, elements.length, null)
    val sample = Array.ofDim[Int](n)
    var j = 0
    val length = elements.length
    for (i <- 0 until length if assistant.sampleNextElement()) { sample(j) = elements(i); j += 1}
    sample
  }

  /**
   * Tests the methods of this class. To do benchmarking, comment the lines
   * printing stuff to the console.
   */
  def test(n: Long, N: Long) {
    val assistant = new DoubleRandomSamplingAssistant(n, N, null)
    val sample = new cern.colt.list.tlong.LongArrayList(n.toInt)
    val timer = new cern.colt.Timer().start()
    for (i <- 0 until N if assistant.sampleNextElement()) {
      sample.add(i)
    }
    timer.stop().display()
    println("sample=" + sample)
    println("Good bye.\n")
  }

  /**
   * Tests the methods of this class. To do benchmarking, comment the lines
   * printing stuff to the console.
   */
  def testArraySampling(n: Int, N: Int) {
    val elements = Array.ofDim[Int](N)
    for (i <- 0 until N) elements(i) = i
    val timer = new cern.colt.Timer().start()
    val sample = sampleArray(n, elements)
    timer.stop().display()
    println("Good bye.\n")
  }
}

/**
 * Conveniently computes a stable <i>Simple Random Sample Without Replacement
 * (SRSWOR)</i> subsequence of <tt>n</tt> elements from a given input sequence
 * of <tt>N</tt> elements; Example: Computing a sublist of <tt>n=3</tt> random
 * elements from a list <tt>(1,...,50)</tt> may yield the sublist
 * <tt>(7,13,47)</tt>. The subsequence is guaranteed to be <i>stable</i>, i.e.
 * elements never change position relative to each other. Each element from the
 * <tt>N</tt> elements has the same probability to be included in the <tt>n</tt>
 * chosen elements. This class is a convenience adapter for
 * <tt>RandomSampler</tt> using blocks.
 *
 * @see DoubleRandomSampler
 * @author wolfgang.hoschek@cern.ch
 * @version 1.0, 02/05/99
 */
@SerialVersionUID(1L)
class DoubleRandomSamplingAssistant(protected var n: Long, N: Long, randomGenerator: DoubleRandomEngine)
    extends cern.colt.PersistentObject {

  protected var sampler: DoubleRandomSampler = new DoubleRandomSampler(n, N, 0, randomGenerator)

  protected var buffer: Array[Long] = new Array[Long](Math.min(n, DoubleRandomSamplingAssistant.MAX_BUFFER_SIZE).toInt)

  protected var bufferPosition: Int = _

  protected var skip: Long = _

  if (n > 0) this.buffer(0) = -1

  fetchNextBlock()

  /**
   * Returns a deep copy of the receiver.
   */
  override def clone(): AnyRef = {
    val copy = super.clone().asInstanceOf[DoubleRandomSamplingAssistant]
    copy.sampler = this.sampler.clone().asInstanceOf[DoubleRandomSampler]
    copy
  }

  /**
   * Not yet commented.
   */
  protected def fetchNextBlock() {
    if (n > 0) {
      val last = buffer(bufferPosition)
      sampler.nextBlock(Math.min(n, DoubleRandomSamplingAssistant.MAX_BUFFER_SIZE).toInt, buffer, 0)
      skip = buffer(0) - last - 1
      bufferPosition = 0
    }
  }

  /**
   * Returns the used random generator.
   */
  def getRandomGenerator: DoubleRandomEngine = this.sampler.my_RandomGenerator

  /**
   * Returns whether the next element of the input sequence shall be sampled
   * (picked) or not.
   *
   * @return <tt>true</tt> if the next element shall be sampled (picked),
   *         <tt>false</tt> otherwise.
   */
  def sampleNextElement(): Boolean = {
    if (n == 0) return false
    if (skip > 0) return false
    skip -= 1
    n -= 1
    if (bufferPosition < buffer.length - 1) {
      skip = buffer(bufferPosition + 1) - buffer(bufferPosition)
      bufferPosition += 1
      skip
    } else {
      fetchNextBlock()
    }
    true
  }
}
