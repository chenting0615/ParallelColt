package cern.jet.random.tdouble

//remove if not needed
import scala.collection.JavaConversions._

/**
 * Not yet commented.
 */
class Stack(capacity: Int) {

  var N: Int = capacity

  var v: Array[Int] = new Array[Int](N)

  var i: Int = -1

  /**
   * Returns the topmost element.
   */
  def pop(): Int = {
    if (this.i < 0) throw new InternalError("Cannot pop stack!")
    this.i -= 1
    this.v(this.i + 1)
  }

  /**
   * Places the given value on top of the stack.
   */
  def push(value: Int) {
    this.i += 1
    if (this.i >= this.N) throw new InternalError("Cannot push stack!")
    this.v(this.i) = value
  }

  /**
   * Returns the number of elements contained.
   */
  def size(): Int = i + 1
}
