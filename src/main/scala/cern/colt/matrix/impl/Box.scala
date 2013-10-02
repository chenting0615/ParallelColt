package cern.colt.matrix.impl

import java.{lang => jl}
import java.{util => ju}
import it.unimi.dsi.fastutil.longs._

trait Box[R] {
  type Raw = R
  type Wrap <: AnyRef
  //type SetType <: ju.Set[Wrap]
  def name : String
  def bound : String
  def tParam : Boolean
}

case class Boxed[R, W <: AnyRef](val name: String, val bound: String= "", val tParam: Boolean = false) extends Box[R]  {
  type Wrap = W
}

object Box {
  implicit object int extends Boxed[Int, jl.Integer]("Int")
  implicit object long extends Boxed[Long, jl.Long] ("Long")
  implicit object double extends Boxed[Double, jl.Double]("Double")
  implicit object float extends Boxed[Float, jl.Float]("Float")
  implicit def ref [R <: AnyRef] = new Boxed[R, jl.Object]("Object", " <: AnyRef", true)
}
