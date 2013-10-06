package cern.colt.matrix.impl

import language.experimental.macros
import scala.reflect.macros.Context

import java.{lang => jl}
import java.{util => ju}
import it.unimi.dsi.fastutil.longs._


trait MapWrap[@specialized Key, @specialized Value, BoxedKey <: AnyRef, BoxedValue <: AnyRef] extends Any {
  type MapTrim = java.util.Map[BoxedKey, BoxedValue] {def trim() : Boolean}
  def m: MapTrim
  final def trim() = m.trim()
  final def clear() = m.clear()
  final def putAll(m2: MapWrap[Key, Value, BoxedKey, BoxedValue]) = m.putAll(m2.m)
  final def size() = m.size()
  final def keySet() = m.keySet()
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

  implicit def fastUtilMap[@specialized K, @specialized V, BK <: AnyRef, BV <: AnyRef](implicit kbk: Boxed[K, BK], vbv: Boxed[V, BV]) : FastUtilMap[K, V]= macro fastUtilMap_impl[K, V, BK, BV]

  def fastUtilMap_impl[@specialized K, @specialized V, BK <: AnyRef, BV <: AnyRef](c: Context)
  (kbk: c.Expr[cern.colt.matrix.impl.Boxed[K, BK]], vbv: c.Expr[cern.colt.matrix.impl.Boxed[V, BV]])
  (implicit k: c.WeakTypeTag[K], v: c.WeakTypeTag[V], bk: c.WeakTypeTag[BK], bv: c.WeakTypeTag[BV])  : c.Expr[FastUtilMap[K, V]]= {
    import c.universe._

    def nameFrag(tp: Type) = if (tp <:< typeOf[AnyVal]) tp.normalize.typeSymbol.name.toString else "Object"
    val Key = k.tpe.normalize
    val Value = v.tpe.normalize
    val BoxedKey =  bk.tpe.normalize
    val BoxedValue = bv.tpe.normalize
    val tParams = Seq(Key,Value).filter(_ <:< typeOf[AnyRef])
    val Pkg = newTermName(s"${nameFrag(Key)}s".toLowerCase)
    val FUMap0 = newTypeName(s"${nameFrag(Key)}2${nameFrag(Value)}OpenHashMap")
    val FUMap1 = tq"it.unimi.dsi.fastutil.${Pkg}.${FUMap0}"
    val FUMap =  if (tParams.isEmpty)  tq"${FUMap1}" else tq"${FUMap1}[..${tParams}]"

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
      new MapType(new FUMapType(initialCapacity, loadFactor))
    }
  }""")
  }

}
