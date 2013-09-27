package cern.colt

import Timer._
//remove if not needed
import scala.collection.JavaConversions._

object Timer {

  /**
   * Shows how to use a timer in convenient ways.
   */
  def test(size: Int) {
    val t = new Timer().start()
    var j = 0
    for (i <- 0 until size) {
      j += 1
    }
    t.stop()
    t.display()
    println("I finished the test using " + t)
    j = 0
    for (i <- 0 until size) {
      j += 1
    }
    t.start()
    j = 0
    for (i <- 0 until size) {
      j += 1
    }
    t.stop().display()
    t.reset()
    t.start()
    j = 0
    for (i <- 0 until size) {
      j += 1
    }
    t.stop().display()
  }
}

/**
 * A handy stopwatch for benchmarking. Like a real stop watch used on ancient
 * running tracks you can start the watch, stop it, start it again, stop it
 * again, display the elapsed time and reset the watch.
 *
 * @author wolfgang.hoschek@cern.ch
 * @author Piotr Wendykier (piotr.wendykier@gmail.com)
 */
class Timer {

  private var baseTime: Long = _

  private var elapsedTime: Long = _

  this.reset()

  /**
   * Prints the elapsed time on System.out
   *
   * @return <tt>this</tt> (for convenience only).
   */
  def display(): Timer = {
    println(this)
    this
  }

  /**
   * Same as <tt>seconds()</tt>.
   */
  def elapsedTime(): Double = seconds()

  /**
   * Returns the elapsed time in milli seconds; does not stop the timer, if
   * started.
   */
  def millis(): Double = {
    var elapsed = elapsedTime
    if (baseTime != 0) {
      elapsed += System.nanoTime() - baseTime
    }
    elapsed / 1000000.0
  }

  /**
   * Returns the elapsed time in nano seconds; does not stop the timer, if
   * started.
   */
  def nanos(): Long = {
    var elapsed = elapsedTime
    if (baseTime != 0) {
      elapsed += System.nanoTime() - baseTime
    }
    elapsed
  }

  /**
   * <tt>T = this - other</tt>; Constructs and returns a new timer which is
   * the difference of the receiver and the other timer. The new timer is not
   * started.
   *
   * @param other
   *            the timer to subtract.
   * @return a new timer.
   */
  def minus(other: Timer): Timer = {
    val copy = new Timer()
    copy.elapsedTime = nanos() - other.nanos()
    copy
  }

  /**
   * Returns the elapsed time in minutes; does not stop the timer, if started.
   */
  def minutes(): Double = seconds() / 60.0

  /**
   * <tt>T = this + other</tt>; Constructs and returns a new timer which is
   * the sum of the receiver and the other timer. The new timer is not
   * started.
   *
   * @param other
   *            the timer to add.
   * @return a new timer.
   */
  def plus(other: Timer): Timer = {
    val copy = new Timer()
    copy.elapsedTime = nanos() + other.nanos()
    copy
  }

  /**
   * Resets the timer.
   *
   * @return <tt>this</tt> (for convenience only).
   */
  def reset(): Timer = {
    elapsedTime = 0
    baseTime = 0
    this
  }

  /**
   * Returns the elapsed time in seconds; does not stop the timer, if started.
   */
  def seconds(): Double = (nanos()) / 1000000000.0

  /**
   * Starts the timer.
   *
   * @return <tt>this</tt> (for convenience only).
   */
  def start(): Timer = {
    baseTime = System.nanoTime()
    this
  }

  /**
   * Stops the timer. You can start it again later, if necessary.
   *
   * @return <tt>this</tt> (for convenience only).
   */
  def stop(): Timer = {
    if (baseTime != 0) {
      elapsedTime = elapsedTime + (System.nanoTime() - baseTime)
    }
    baseTime = 0
    this
  }

  /**
   * Returns a String representation of the receiver.
   */
  override def toString(): String = {
    "Time=" + String.format("%.4f", this.elapsedTime()) + 
      " secs"
  }
}
