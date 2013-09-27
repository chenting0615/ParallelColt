package cern.jet.random.tdouble

//remove if not needed
import scala.collection.JavaConversions._

/**
 * Abstract base class for all discrete distributions.
 *
 * @author wolfgang.hoschek@cern.ch
 * @version 1.0, 09/24/99
 */
@SerialVersionUID(1L)
abstract class AbstractDiscreteDistribution protected () extends AbstractDoubleDistribution {

  /**
   * Returns a random number from the distribution; returns
   * <tt>(double) nextInt()</tt>.
   */
  def nextDouble(): Double = nextInt()

  /**
   * Returns a random number from the distribution.
   */
  def nextInt(): Int
}
