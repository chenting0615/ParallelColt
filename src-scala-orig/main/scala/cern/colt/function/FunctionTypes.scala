package cern.colt.function

/**
  */
object FunctionTypes {

  type DoubleDoubleFunction = cern.colt.function.Function2[Double, Double, Double]

  type LongDoubleFunction = cern.colt.function.Function2[Long, Double, Double]

  type IntFunction = cern.colt.function.Function1[Int]

  type IntIntFunction = cern.colt.function.Function2[Int, Int, Int]

  type IntIntIntFunction = cern.colt.function.Function3[Int, Int, Int, Int]

  type IntIntDoubleFunction = cern.colt.function.Function3[Int, Int, Double, Double]
}
