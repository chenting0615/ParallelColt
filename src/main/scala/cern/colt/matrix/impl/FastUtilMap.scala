package cern.colt.matrix.impl

import java.{lang => jl}
import java.{util => ju}
import it.unimi.dsi.fastutil.longs._


trait MapWrap[@specialized Key, @specialized Value, BoxedKey <: AnyRef, BoxedValue <: AnyRef] /*extends Any*/ {
  //type MapTrim = java.util.Map[BoxedKey, BoxedValue] {def trim() : Boolean}
  @inline def m: MapTrim [BoxedKey, BoxedValue]
  def trim() = m.trim()
  def clear() = m.clear()
  def putAll(m2: MapWrap[Key, Value, BoxedKey, BoxedValue]) = m.putAll(m2.m)
  def size() = m.size()
  def keySet() = m.keySet()
  @inline def get(k: Key) : Value
  @inline def put(k: Key, v: Value): Value
  def remove(k: Key) : Value
}

abstract class FastUtilMap[@specialized K, @specialized V] {
  type Key = K
  type Value = V
  type BoxedKey <: AnyRef
  type BoxedValue <: AnyRef

  //type MapTrim = java.util.Map[BoxedKey, BoxedValue] {def trim() : Boolean}
  type Wrap = MapWrap[Key, Value, BoxedKey, BoxedValue]
  type FUMapType <: MapTrim[BoxedKey, BoxedValue]
  type MapType <: Wrap

  def createMap(initialCapacity: Int, loadFactor: Float): MapType
}

object FastUtilMap {

  val m: ju.Map[java.lang.Long, java.lang.Double]  =  new Long2DoubleOpenHashMap()

  implicit object LongDouble extends FastUtilMap[Long, Double] {
    type BoxedKey = jl.Long
    type BoxedValue = jl.Double
    type FUMapType = Long2DoubleOpenHashMap

    final class MapType(val m: FUMapType) extends /*AnyVal with*/ Wrap {
      def get(k: Key) : Value = m.get(k)
      def put(k: Key, v: Value): Value = m.put(k, v)
      def remove(k: Key) : Value = m.remove()
    }

    def createMap(initialCapacity: Int, loadFactor: Float) = {
      new MapType(new Long2DoubleOpenHashMap(initialCapacity, loadFactor))
    }
  }

  implicit object LongFloat extends FastUtilMap[Long, Float] {
    type BoxedKey = jl.Long
    type BoxedValue = jl.Float
    type FUMapType = Long2FloatOpenHashMap

    final class MapType(val m: FUMapType) extends /*AnyVal with*/ Wrap {
      def get(k: Key) : Value = m.get(k)
      def put(k: Key, v: Value): Value = m.put(k, v)
      def remove(k: Key) : Value = m.remove()
    }

    def createMap(initialCapacity: Int, loadFactor: Float) = {
      new MapType(new Long2FloatOpenHashMap(initialCapacity, loadFactor))
    }
  }

  implicit object LongLong extends FastUtilMap[Long, Long] {
    type BoxedKey = jl.Long
    type BoxedValue = jl.Long
    type FUMapType = Long2LongOpenHashMap

    final class MapType(val m: FUMapType) extends /*AnyVal with*/ Wrap {
      def get(k: Key) : Value = m.get(k)
      def put(k: Key, v: Value): Value = m.put(k, v)
      def remove(k: Key) : Value = m.remove()
    }

    def createMap(initialCapacity: Int, loadFactor: Float) = {
      new MapType(new Long2LongOpenHashMap(initialCapacity, loadFactor))
    }
  }

  implicit object LongInt extends FastUtilMap[Long, Int] {
    type BoxedKey = jl.Long
    type BoxedValue = jl.Integer
    type FUMapType = Long2IntOpenHashMap

    final class MapType(val m: FUMapType) extends /*AnyVal with*/ Wrap {
      def get(k: Key) : Value = m.get(k)
      def put(k: Key, v: Value): Value = m.put(k, v)
      def remove(k: Key) : Value = m.remove()
    }

    def createMap(initialCapacity: Int, loadFactor: Float) = {
      new MapType(new Long2IntOpenHashMap(initialCapacity, loadFactor))
    }
  }

  trait LongObject[V<: AnyRef] extends FastUtilMap[Long, V] {
    type BoxedKey = jl.Long
    type BoxedValue = Value
    type FUMapType = Long2ObjectOpenHashMap[Value]

    final class MapType(val m: FUMapType) extends Wrap {
      def get(k: Key) : Value = m.get(k)
      def put(k: Key, v: Value): Value = m.put(k, v)
      def remove(k: Key) : Value = m.remove()
    }

    def createMap(initialCapacity: Int, loadFactor: Float) = {
      new MapType(new Long2ObjectOpenHashMap[Value](initialCapacity, loadFactor))
    }
  }

  implicit def LongObject[V<: AnyRef] = new LongObject[V]{}
}
