package cern.colt.matrix.impl

import java.{lang => jl}
import java.{util => ju}
import it.unimi.dsi.fastutil.longs._

trait Box[R] {
  type Raw = R
  type Wrap
  type SetType <: ju.Set[Wrap]
  implicit def box(x: Raw) : Wrap
  implicit def unbox(x: Wrap) : Raw
}

abstract class Boxed[R, W](implicit val bx: R => W, val ubx: W => R) extends Box[R]  {
  //type Raw = R
  type Wrap = W
  implicit def box(x: Raw) : Wrap = bx(x)
  implicit def unbox(x: Wrap) : Raw = ubx(x)
}

object Box {
  implicit case object int extends Boxed[Int, jl.Integer]
  implicit case object long extends Boxed[Long, jl.Long]
  implicit case object double extends Boxed[Double, jl.Double]
  implicit case object float extends Boxed[Float, jl.Float]
  implicit case object ref extends Boxed[AnyRef, jl.Object]
}

abstract class FastUtilMap[K, V](implicit val bk: Box[K], val bv: Box[V]) {
  type Key = K
  type Value = V
  type BK // = bk.Wrap
  type BV // = bv.Wrap

  type MapType <: ju.Map[BK, BV]{def trim() : Boolean}

  def createMap(initialCapacity: Int, loadFactor: Float): MapType

  trait ImpBase {
    implicit def boxKey(key: Key) : BK
//    implicit def unboxKey(key: BK) : Key // = bk.unbox(key)
    implicit def boxValue(value: Value) : BV
    implicit def unboxValue(value: BV) : Value
  }

  val Implicits : ImpBase
}

object FastUtilMap {

  val m: ju.Map[java.lang.Long, java.lang.Double]  =  new Long2DoubleOpenHashMap()

  implicit object LongDouble extends FastUtilMap[Long, Double] {
    type BK = jl.Long
    type BV = jl.Double

    type MapType = Long2DoubleOpenHashMap

    def createMap(initialCapacity: Int, loadFactor: Float) = {
      new Long2DoubleOpenHashMap(initialCapacity, loadFactor)
    }
    object Implicits extends ImpBase {
      implicit def boxKey(key: Key) : BK = long2Long(key)
      implicit def boxValue(value: Value) : BV = double2Double(value)
      implicit def unboxValue(value: BV) : Value = Double2double(value)
    }

  }

  implicit object LongFloat extends FastUtilMap[Long, Float] {
    type BK = jl.Long
    type BV = jl.Float

    type MapType = Long2FloatOpenHashMap

    def createMap(initialCapacity: Int, loadFactor: Float) = {
      new Long2FloatOpenHashMap(initialCapacity, loadFactor)
    }
    object Implicits extends ImpBase {
      implicit def boxKey(key: Key) : BK = long2Long(key)
      implicit def boxValue(value: Value) : BV = float2Float(value)
      implicit def unboxValue(value: BV) : Value = Float2float(value)
    }
  }

  implicit object LongLong extends FastUtilMap[Long, Long] {
    type BK = jl.Long
    type BV = jl.Long

    type MapType = Long2LongOpenHashMap

    def createMap(initialCapacity: Int, loadFactor: Float) = {
      new Long2LongOpenHashMap(initialCapacity, loadFactor)
    }
    object Implicits extends ImpBase {
      implicit def boxKey(key: Key) : BK = long2Long(key)
      implicit def boxValue(value: Value) : BV = long2Long(value)
      implicit def unboxValue(value: BV) : Value = Long2long(value)
    }
  }

  implicit object LongInt extends FastUtilMap[Long, Int] {
    type BK = jl.Long
    type BV = jl.Integer

    type MapType = Long2IntOpenHashMap

    def createMap(initialCapacity: Int, loadFactor: Float) = {
      new Long2IntOpenHashMap(initialCapacity, loadFactor)
    }
    object Implicits extends ImpBase {
      implicit def boxKey(key: Key) : BK = long2Long(key)
      implicit def boxValue(value: Value) : BV = int2Integer(value)
      implicit def unboxValue(value: BV) : Value = Integer2int(value)
    }
  }

  implicit object LongObject extends FastUtilMap[Long, AnyRef] {
    type BK = jl.Long
    type BV = jl.Object

    type MapType = Long2ObjectOpenHashMap[AnyRef]

    def createMap(initialCapacity: Int, loadFactor: Float) = {
      new Long2ObjectOpenHashMap[AnyRef](initialCapacity, loadFactor)
    }
    object Implicits extends ImpBase {
      implicit def boxKey(key: Key) : BK = long2Long(key)
      implicit def boxValue(value: Value) : BV = value
      implicit def unboxValue(value: BV) : Value = value
    }
  }

}
