package cern.colt

/**
 * Interface for an object that knows how to swap elements at two positions (a,b).
 *
 * @see cern.colt.GenericSorting
 *
 * @author wolfgang.hoschek@cern.ch
 * @version 1.0, 03-Jul-99
 */
trait Swapper {

  /**
   * Swaps the generic data g[a] with g[b].
   */
  def swap(a: Int, b: Int): Unit
}
