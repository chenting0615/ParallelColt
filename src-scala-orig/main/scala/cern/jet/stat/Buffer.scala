package cern.jet.stat

//remove if not needed
import scala.collection.JavaConversions._

/**
 * A buffer holding elements; internally used for computing approximate
 * quantiles.
 */
@SerialVersionUID(1L)
abstract class Buffer(var k: Int) extends cern.colt.PersistentObject {

  var weight: Int = 1

  var level: Int = 0

  var isAllocated: Boolean = false

  /**
   * Clears the receiver.
   */
  def clear(): Unit

  /**
   * Returns whether the receiver is already allocated.
   */
  def isAllocated(): Boolean = isAllocated

  /**
   * Returns whether the receiver is empty.
   */
  def isEmpty(): Boolean

  /**
   * Returns whether the receiver is empty.
   */
  def isFull(): Boolean

  /**
   * Returns whether the receiver is partial.
   */
  def isPartial(): Boolean = !(isEmpty || isFull)

  /**
   * Returns whether the receiver's level.
   */
  def level(): Int = level

  /**
   * Sets the receiver's level.
   */
  def level(level: Int) {
    this.level = level
  }

  /**
   * Returns the number of elements contained in the receiver.
   */
  def size(): Int

  /**
   * Sorts the receiver.
   */
  def sort(): Unit

  /**
   * Returns whether the receiver's weight.
   */
  def weight(): Int = weight

  /**
   * Sets the receiver's weight.
   */
  def weight(weight: Int) {
    this.weight = weight
  }
}
