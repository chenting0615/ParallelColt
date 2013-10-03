package cern.colt.matrix

import cern.colt.function.ProcedureTypes._
import hep.aida.bin.StaticBin1D
import cern.colt.function.{Procedure2, Procedure1}
import cern.colt.matrix.MatrixTypes._

/**
  */
trait MatrixAlgebra1D[T] extends MatrixAlgebra[T, Matrix1D] {

  def assignByIndex(func: Function1[Int, T]): Matrix1D[T]

  def aggregateStats: StaticBin1D[T]

  def toDiagonal: Matrix2D[T]

  def take(indexes: IntMatrix1D)

  def viewEqualValues(other: Matrix1D[T])

  def viewNotEqualValues(other: Matrix1D[T])

  /**
   * Constructs and returns a new <i>selection view</i> that is a matrix
   * holding all <b>elements</b> whose index matches the given condition. Applies the
   * condition to each element and takes only those element where
   * <tt>condition.apply(i)</tt> yields <tt>true</tt>.
   * @param condition
   * The condition to be matched.
   * @return the new view.
   */
  def viewIndexSelection(condition: IntProcedure)

  /**
   * Constructs and returns a new <i>selection view</i> that is a matrix
   * holding all <b>columns</b> whose value matches the given condition. Applies the
   * condition to each column value and takes only those columns where
   * <tt>condition.apply(i)</tt> yields <tt>true</tt>. To match
   * columns, use a dice view.
   * @param condition
   * The condition to be matched.
   * @return the new view.
   */
  def viewColumnSelection(condition: Procedure1[T]): Matrix1D[T]

  /**
   * Constructs and returns a new <i>selection view</i> that is a matrix
   * holding all <b>columns</b> whose index and value match the given condition. Applies the
   * condition to each column value and takes only those columns where
   * <tt>condition.apply(i)</tt> yields <tt>true</tt>. To match
   * columns, use a dice view.
   * @param condition
   * The condition to be matched.
   * @return the new view.
   */
  def viewColumnSelection(condition: Procedure2[Int, T]): Matrix1D[T]
}
