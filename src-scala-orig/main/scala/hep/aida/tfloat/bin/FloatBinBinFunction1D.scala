package hep.aida.tfloat.bin

//remove if not needed
import scala.collection.JavaConversions._

/**
 * Interface that represents a function object: a function that takes two bins
 * as arguments and returns a single value.
 */
trait FloatBinBinFunction1D {

  /**
   * Applies a function to two bin arguments.
   *
   * @param x
   *            the first argument passed to the function.
   * @param y
   *            the second argument passed to the function.
   * @return the result of the function.
   */
  def apply(x: DynamicFloatBin1D, y: DynamicFloatBin1D): Float
}
