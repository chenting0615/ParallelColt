package cern.colt.function

/**
 * Interface that represents a function object: a function that takes 27
 * arguments and returns a single value.
 */
@specialized
trait Function27[T] {

  /**
   * Applies a function to 27 arguments.
   *
   * @return the result of the function.
   */
  def apply(a000: T,
      a001: T,
      a002: T,
      a010: T,
      a011: T,
      a012: T,
      a020: T,
      a021: T,
      a022: T,
      a100: T,
      a101: T,
      a102: T,
      a110: T,
      a111: T,
      a112: T,
      a120: T,
      a121: T,
      a122: T,
      a200: T,
      a201: T,
      a202: T,
      a210: T,
      a211: T,
      a212: T,
      a220: T,
      a221: T,
      a222: T): T
}
