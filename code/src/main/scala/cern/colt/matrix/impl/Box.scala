//package cern.colt.matrix.impl
//
//import java.{lang => jl}
//import java.{util => ju}
//import it.unimi.dsi.fastutil.longs._
//
//trait Box[R] {
//  type Raw = R
//  type Wrap
//  type SetType <: ju.Set[Wrap]
//  implicit def box(x: Raw) : Wrap
//  implicit def unbox(x: Wrap) : Raw
//}
//
//abstract class Boxed[R, W](implicit val bx: R => W, val ubx: W => R) extends Box[R]  {
//  //type Raw = R
//  type Wrap = W
//  implicit def box(x: Raw) : Wrap = bx(x)
//  implicit def unbox(x: Wrap) : Raw = ubx(x)
//}
//
//object Box {
//  implicit case object int extends Boxed[Int, jl.Integer]
//  implicit case object long extends Boxed[Long, jl.Long]
//  implicit case object double extends Boxed[Double, jl.Double]
//  implicit case object float extends Boxed[Float, jl.Float]
//  implicit case object ref extends Boxed[AnyRef, jl.Object]
//}
