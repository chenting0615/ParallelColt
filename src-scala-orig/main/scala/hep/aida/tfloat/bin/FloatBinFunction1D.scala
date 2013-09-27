package hep.aida.tfloat.bin

//remove if not needed
import scala.collection.JavaConversions._

/**
 * Interface that represents a function object: a function that takes two bins
 * as arguments and returns a single value.
 */
trait FloatBinFunction1D {

  /**
   * Applies a function to one bin argument.
   *
   * @param x
   *            the argument passed to the function.
   * @return the result of the function.
   */
  def apply(x: DynamicFloatBin1D): Float

  /**
   * Returns the name of this function.
   *
   * @return the name of this function.
   */
  def name(): String
}
