package cern.colt.matrix.impl

import language.experimental.macros
import scala.reflect.macros.Context

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

abstract class FastUtilMap[@specialized K, @specialized V] {
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

  implicit def fastUtilMap[K, V, BK <: AnyRef, BV <: AnyRef](implicit kbk: Boxed[K, BK], vbv: Boxed[V, BV]) : FastUtilMap[K, V]= macro fastUtilMap_impl[K, V, BK, BV]

  def fastUtilMap_impl[K, V, BK <: AnyRef, BV <: AnyRef](c: Context)
  (kbk: c.Expr[cern.colt.matrix.impl.Boxed[K, BK]], vbv: c.Expr[cern.colt.matrix.impl.Boxed[V, BV]])
  (implicit k: c.WeakTypeTag[K], v: c.WeakTypeTag[V], bk: c.WeakTypeTag[BK], bv: c.WeakTypeTag[BV])  : c.Expr[FastUtilMap[K, V]]= {
    import c.universe._
    val Key = ??? // ...k.tpe...
    val Value = ??? // ...v.tpe...
    val BoxedKey =  ???// ...bk.tpe...
    val BoxedValue = ??? // ...bv.tpe  ...
    val FUMap =  ???   // ${KeyNameFragment}2${ValNameFragment}OpenHashMap or ${KeyNameFragment}2${ValNameFragment}OpenHashMap[$Value] if Value <: AnyRef
    c.Expr[FastUtilMap[K, V]](q"""new cern.colt.matrix.impl.FastUtilMap[${Key}, ${Value}] {
    type BoxedKey = ${BoxedKey}
    type BoxedValue = ${BoxedValue}
    type FUMapType = ${FUMap}

    final class MapType(final val m: FUMapType) extends Wrap {
      def get(k: Key) : Value = m.get(k)
      def put(k: Key, v: Value): Value = m.put(k, v)
      def remove(k: Key) : Value = m.remove()
    }

    def createMap(initialCapacity: Int, loadFactor: Float) = {
      new MapType(new ${FUMap}(initialCapacity, loadFactor))
    }
  }""")
  }

}
