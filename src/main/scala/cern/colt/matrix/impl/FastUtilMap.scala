package cern.colt.matrix.impl

import java.{lang => jl}
import java.{util => ju}
import it.unimi.dsi.fastutil.longs._

trait MapWrap[@specialized Key, @specialized Value, BoxedKey <: AnyRef, BoxedValue <: AnyRef] extends Any {
  type MapTrim = java.util.Map[BoxedKey, BoxedValue] {def trim() : Boolean}
  def m: MapTrim
  def trim() = m.trim()
  def clear() = m.clear()
  def putAll(m2: MapWrap[Key, Value, BoxedKey, BoxedValue]) = m.putAll(m2.m)
  def size() = m.size()
  def keySet() = m.keySet()
  def get(k: Key) : Value
  def put(k: Key, v: Value): Value
  def remove(k: Key) : Value
}

trait FastUtilMap[@specialized K, @specialized V] {
  type Key = K
  type Value = V
  type BoxedKey <: AnyRef
  type BoxedValue <: AnyRef

  type Wrap = MapWrap[Key, Value, BoxedKey, BoxedValue]
  type FUMapType <: Wrap#MapTrim
  type MapType <: Wrap

  def createMap(initialCapacity: Int, loadFactor: Float): MapType
}

object FastUtilMap {

  implicit def FastUtilMap[K, V, BV <: AnyRef, BK <: AnyRef](implicit bk: Boxed[K,  BK], bv: Boxed[V, BV]) =  new FastUtilMap[K, V] {
    type BoxedKey = BK
    type BoxedValue = BV
    type FUMapType = Wrap#MapTrim

    final class MapType(val m: FUMapType) extends /*AnyVal with*/ Wrap {
      def get(k: Key) : Value = ??? // m.get(k)
      def put(k: Key, v: Value): Value = ??? // m.put(k, v)
      def remove(k: Key) : Value = ??? // m.remove()
    }

    def createMap(initialCapacity: Int, loadFactor: Float) = {
      def newFUMap: FUMapType= ???
      new MapType(newFUMap)
    }
  }

}
