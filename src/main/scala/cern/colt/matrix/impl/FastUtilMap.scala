package cern.colt.matrix.impl

/*
import java.{lang => jl}
import java.{util => ju}
import cern.colt.map.impl.OpenHashMap
*/

abstract class FastUtilMap[K, V] {
/*
  type Key = K
  type Value = V
  type BoxedKey
  type BoxedValue

  type MapType <: ju.Map[BoxedKey, BoxedValue]{def trimToSize() : Boolean}

  def createMap(initialCapacity: Int, loadFactor: Float): MapType
  @inline def get(map: MapType, key: Key) : Value
  @inline def put(map: MapType, key: Key, value: Value) : Boolean
  // TODO put & get are the minimum needed to compile but other specialized methods should also be handled here
*/

}

object FastUtilMap {

/*
  implicit object LongDouble extends FastUtilMap[Long, Double] {
    type BoxedKey = jl.Long
    type BoxedValue = jl.Double

    type MapType = OpenHashMap[Long, Double]

    def createMap(initialCapacity: Int, loadFactor: Float) = {
      new OpenHashMap[Long, Double](initialCapacity, loadFactor, loadFactor)
    }

    @inline def get(map: MapType, key: Key) : Value  = map.get(key)
    @inline def put(map: MapType, key: Key, value: Value) : Boolean =  map.put(key, value)

  }

  implicit object LongFloat extends FastUtilMap[Long, Float] {
    type BoxedKey = jl.Long
    type BoxedValue = jl.Float

    type MapType = OpenHashMap[Long, Float]

    def createMap(initialCapacity: Int, loadFactor: Float) = {
      new OpenHashMap[Long, Float](initialCapacity, loadFactor, loadFactor)
    }

    @inline def get(map: MapType, key: Key) : Value  = map.get(key)
    @inline def put(map: MapType, key: Key, value: Value) : Boolean =  map.put(key, value)
  }

  implicit object LongLong extends FastUtilMap[Long, Long] {
    type BoxedKey = jl.Long
    type BoxedValue = jl.Long

    type MapType = OpenHashMap[Long, Long]

    def createMap(initialCapacity: Int, loadFactor: Float) = {
      new OpenHashMap[Long, Long](initialCapacity, loadFactor, loadFactor)
    }

    @inline def get(map: MapType, key: Key) : Value  = map.get(key)
    @inline def put(map: MapType, key: Key, value: Value) : Boolean =  map.put(key, value)
  }

  implicit object LongInt extends FastUtilMap[Long, Int] {
    type BoxedKey = jl.Long
    type BoxedValue = jl.Integer

    type MapType = OpenHashMap[Long, Int]

    def createMap(initialCapacity: Int, loadFactor: Float) = {
      new OpenHashMap[Long, Int](initialCapacity, loadFactor, loadFactor)
    }

    def get(map: MapType, key: Key) : Value  = map.get(key)
    def put(map: MapType, key: Key, value: Value) : Boolean =  map.put(key, value)
  }

  implicit object LongObject extends FastUtilMap[Long, AnyRef] {
    type BoxedKey = jl.Long
    type BoxedValue = jl.Object

    type MapType = OpenHashMap[Long, Object]

    def createMap(initialCapacity: Int, loadFactor: Float) = {
      new OpenHashMap[Long, Object](initialCapacity, loadFactor, loadFactor)
    }

    @inline def get(map: MapType, key: Key) : Value  = map.get(key)
    @inline def put(map: MapType, key: Key, value: Value) : Boolean =  map.put(key, value)
  }
*/

}
