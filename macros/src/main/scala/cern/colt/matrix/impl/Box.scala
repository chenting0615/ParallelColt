package cern.colt.matrix.impl

import java.{lang => jl}
import java.{util => ju}
import it.unimi.dsi.fastutil.longs._

trait Box[@specialized R] {
  type Raw = R
  type Wrap <: AnyRef
  //type SetType <: ju.Set[Wrap]
}

class Boxed[@specialized R, W <: AnyRef] extends Box[R]  {
  type Wrap = W
}

object Box {
  implicit object int extends Boxed[Int, jl.Integer]
  implicit object long extends Boxed[Long, jl.Long]
  implicit object double extends Boxed[Double, jl.Double]
  implicit object float extends Boxed[Float, jl.Float]
  implicit def ref [@specialized R <: AnyRef] = new Boxed[R, R]
}
