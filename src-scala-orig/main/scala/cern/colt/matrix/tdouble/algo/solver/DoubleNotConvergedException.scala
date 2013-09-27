package cern.colt.matrix.tdouble.algo.solver

import DoubleNotConvergedException._
//remove if not needed
import scala.collection.JavaConversions._

object DoubleNotConvergedException {

  object Reason extends Enumeration {

    val Iterations = new Reason()

    val Divergence = new Reason()

    val Breakdown = new Reason()

    class Reason extends Val

    implicit def convertValue(v: Value): Reason = v.asInstanceOf[Reason]
  }
}

/**
 * Signals lack of convergence of an iterative process
 */
@SerialVersionUID(1L)
class DoubleNotConvergedException(protected var reason: Reason, message: String)
    extends Exception(message) {

  /**
   * Constructor for NotConvergedException. No message is provided
   *
   * @param reason
   *            The reason for the lack of convergence
   */
  def this(reason: Reason) {
    this()
    this.reason = reason
  }

  /**
   * Returns the reason for the exception
   */
  def getReason(): Reason = reason
}
