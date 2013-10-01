package cern.colt.matrix.impl

import language.experimental.macros
import scala.reflect.macros.Context

import java.{lang => jl}
import java.{util => ju}
import it.unimi.dsi.fastutil.longs._

abstract class FastUtilMap[K, V] {
  type Key = K
  type Value = V
  type BoxedKey
  type BoxedValue

  type MapType <: ju.Map[BoxedKey, BoxedValue]{def trim() : Boolean}

  def createMap(initialCapacity: Int, loadFactor: Float): MapType
  @inline def get(map: MapType, key: Key) : Value
  @inline def put(map: MapType, key: Key, value: Value) : Value
  // TODO put & get are the minimum needed to compile but other specialized methods should also be handled here

}

object FastUtilMap {

  implicit def fastUtilMap[K, V] = macro fastUtilMap_impl[K, V]

  def fastUtilMap_impl[K, V](c: Context)(k: c.WeakTypeTag[K], v: c.WeakTypeTag[V]): c.Expr[Int] = {
    import c.universe._
    c.Expr[Int](q"1")
  }

  implicit object LongDouble extends FastUtilMap[Long, Double] {
    type BoxedKey = jl.Long
    type BoxedValue = jl.Double

    type MapType = Long2DoubleOpenHashMap

    def createMap(initialCapacity: Int, loadFactor: Float) = {
      new Long2DoubleOpenHashMap(initialCapacity, loadFactor)
    }

    @inline def get(map: MapType, key: Key) : Value  = map.get(key)
    @inline def put(map: MapType, key: Key, value: Value) : Value =  map.put(key, value)

  }

  implicit object LongFloat extends FastUtilMap[Long, Float] {
    type BoxedKey = jl.Long
    type BoxedValue = jl.Float

    type MapType = Long2FloatOpenHashMap

    def createMap(initialCapacity: Int, loadFactor: Float) = {
      new Long2FloatOpenHashMap(initialCapacity, loadFactor)
    }

    @inline def get(map: MapType, key: Key) : Value  = map.get(key)
    @inline def put(map: MapType, key: Key, value: Value) : Value =  map.put(key, value)
  }

  implicit object LongLong extends FastUtilMap[Long, Long] {
    type BoxedKey = jl.Long
    type BoxedValue = jl.Long

    type MapType = Long2LongOpenHashMap

    def createMap(initialCapacity: Int, loadFactor: Float) = {
      new Long2LongOpenHashMap(initialCapacity, loadFactor)
    }

    @inline def get(map: MapType, key: Key) : Value  = map.get(key)
    @inline def put(map: MapType, key: Key, value: Value) : Value =  map.put(key, value)
  }

  implicit object LongInt extends FastUtilMap[Long, Int] {
    type BoxedKey = jl.Long
    type BoxedValue = jl.Integer

    type MapType = Long2IntOpenHashMap

    def createMap(initialCapacity: Int, loadFactor: Float) = {
      new Long2IntOpenHashMap(initialCapacity, loadFactor)
    }

    def get(map: MapType, key: Key) : Value  = map.get(key)
    def put(map: MapType, key: Key, value: Value) : Value =  map.put(key, value)
  }

  implicit object LongObject extends FastUtilMap[Long, AnyRef] {
    type BoxedKey = jl.Long
    type BoxedValue = jl.Object

    type MapType = Long2ObjectOpenHashMap[AnyRef]

    def createMap(initialCapacity: Int, loadFactor: Float) = {
      new Long2ObjectOpenHashMap[AnyRef](initialCapacity, loadFactor)
    }

    @inline def get(map: MapType, key: Key) : Value  = map.get(key)
    @inline def put(map: MapType, key: Key, value: Value) : Value =  map.put(key, value)
  }

}
