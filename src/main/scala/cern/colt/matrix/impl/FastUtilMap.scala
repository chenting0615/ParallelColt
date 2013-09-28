package cern.colt.matrix.impl

import java.util
import it.unimi.dsi.fastutil.longs._
import java.{lang => jl}

//trait Boxed[Raw, Wrap]//(implicit val box: Raw => Wrap, val unbox: Wrap => Raw)
//
//object Boxed {
//  implicit case object int extends Boxed[Int, jl.Integer]
//  implicit case object long extends Boxed[Long, jl.Long]
//  implicit case object double extends Boxed[Double, jl.Double]
//  implicit case object float extends Boxed[Float, jl.Float]
//  implicit case object ref extends Boxed[AnyRef, jl.Object]
//}
//
//trait FUM[K, V, M] {
//  def createMap(initialCapacity: Int, loadFactor: Float): M
//}
//
//object FUM {
//  implicit object fumLongDouble extends FUM[Long, Double, Long2DoubleOpenHashMap] {
//     def createMap(initialCapacity: Int, loadFactor: Float) = new Long2DoubleOpenHashMap(initialCapacity, loadFactor)
//  }
//  implicit object fumLongInt extends FUM[Long, Int, Long2IntOpenHashMap] {
//     def createMap(initialCapacity: Int, loadFactor: Float) = new Long2IntOpenHashMap(initialCapacity, loadFactor)
//  }
//}
//def zz[K, V, M](implicit bk: Boxed[Key, BoxedKey], bv: Boxed[Value, BoxedValue]):  FastUtilMap[K, V]



trait FastUtilMap[Key, Value]//(implicit bk: Boxed[Key, BoxedKey], bv: Boxed[Value, BoxedValue])
 {
  type Trim = {def trim() : Boolean}
  type BK
  type BV
//  type M[BK <% Key, BV <% Value] = java.util.Map[BK,BV]  ,
  type MapType <: java.util.Map[BK, BV] with Trim //<: java.util.Map[_, _]

  def createMap(initialCapacity: Int, loadFactor: Float): MapType
}

object FastUtilMap {

  val m: java.util.Map[java.lang.Long, java.lang.Double]  =  new Long2DoubleOpenHashMap()

  implicit object LongDouble extends FastUtilMap[Long, Double] {
    type BK = jl.Long
    type BV = jl.Double
    override type MapType = Long2DoubleOpenHashMap

    def createMap(initialCapacity: Int, loadFactor: Float) = {
      new Long2DoubleOpenHashMap(initialCapacity, loadFactor)
    }
  }

  implicit object LongFloat extends FastUtilMap[Long, Float] {
    type MapType = Long2FloatOpenHashMap

    def createMap(initialCapacity: Int, loadFactor: Float) = {
      new Long2FloatOpenHashMap(initialCapacity, loadFactor)
    }
  }

  implicit object LongLong extends FastUtilMap[Long, Long] {
    type MapType = Long2LongOpenHashMap

    def createMap(initialCapacity: Int, loadFactor: Float) = {
      new Long2LongOpenHashMap(initialCapacity, loadFactor)
    }
  }

  implicit object LongInt extends FastUtilMap[Long, Int] {
    type MapType = Long2IntOpenHashMap

    def createMap(initialCapacity: Int, loadFactor: Float) = {
      new Long2IntOpenHashMap(initialCapacity, loadFactor)
    }
  }

  implicit object LongObject extends FastUtilMap[Long, AnyRef] {
    type MapType = Long2ObjectOpenHashMap[AnyRef]

    def createMap(initialCapacity: Int, loadFactor: Float) = {
      new Long2ObjectOpenHashMap[AnyRef](initialCapacity, loadFactor)
    }
  }

}
