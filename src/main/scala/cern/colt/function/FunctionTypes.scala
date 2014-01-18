package cern.colt.function

/**
  */
object FunctionTypes {

  type DoubleFunction = Function1[Double, Double]

  type DoubleDoubleFunction = Function2[Double, Double, Double]

  type FloatFunction = Function1[Float, Float]

  type FloatFloatFunction = Function2[Float, Float, Float]

  type IntDoubleFunction = Function2[Int, Double, Double]

  type LongDoubleFunction = Function2[Long, Double, Double]

  type IntFunction = Function1[Int, Int]

  type IntIntFunction = Function2[Int, Int, Int]

  type IntIntIntFunction = Function3[Int, Int, Int, Int]

  type IntIntDoubleFunction = Function3[Int, Int, Double, Double]
}
