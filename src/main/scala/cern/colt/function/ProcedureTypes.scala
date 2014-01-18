package cern.colt.function

/**
 * Interface that represents a procedure object: a procedure that takes
 * arguments and returns a boolean, indicating whether to continue traversing
 * the matrix or not.
 */
object ProcedureTypes {
  type DoubleProcedure = Procedure1[Double]

  type FloatProcedure = Procedure1[Float]

  type FloatFloatProcedure = Procedure2[Float, Float]

  type IntDoubleProcedure = Procedure2[Int, Double]

  type IntIntDoubleProcedure = Procedure3[Int, Int, Double]

  type LongDoubleProcedure = Procedure2[Long, Double]

  type DoubleDoubleProcedure = Procedure2[Double, Double]

  type DoubleLongProcedure = Procedure2[Double, Long]

  type IntProcedure = Procedure1[Int]

  type IntIntProcedure = Procedure2[Int, Int]

  type IntIntIntProcedure = Procedure3[Int, Int, Int]

}
