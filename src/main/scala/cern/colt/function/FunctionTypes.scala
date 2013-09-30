package cern.colt.function

/**
  */
object FunctionTypes {

  type DoubleDoubleFunction = Function2[Double, Double, Double]

  type IntDoubleFunction = Function2[Int, Double, Double]

  type LongDoubleFunction = Function2[Long, Double, Double]

  type IntFunction = cern.colt.function.Function1[Int]

  type IntIntFunction = Function2[Int, Int, Int]

  type IntIntIntFunction = Function3[Int, Int, Int, Int]

  type IntIntDoubleFunction = Function3[Int, Int, Double, Double]
}
