package cern.colt.matrix.impl

import java.util
import it.unimi.dsi.fastutil.longs._

trait FastUtilMap[IN, OUT] {
  type MapType <: util.Map[IN, OUT]

  def createMap(initialCapacity: Int, loadFactor: Float): MapType

  def trim(map: MapType)
}

object FastUtilMap {

  implicit object LongDouble extends FastUtilMap[Long, Double] {
    type MapType = Long2DoubleOpenHashMap

    def createMap(initialCapacity: Int, loadFactor: Float) = {
      new Long2DoubleOpenHashMap(initialCapacity, loadFactor)
    }

    def trim(map: MapType) {
      map.trim()
    }
  }

  implicit object LongFloat extends FastUtilMap[Long, Float] {
    type MapType = Long2FloatOpenHashMap

    def createMap(initialCapacity: Int, loadFactor: Float) = {
      new Long2FloatOpenHashMap(initialCapacity, loadFactor)
    }

    def trim(map: MapType) {
      map.trim()
    }
  }

  implicit object LongLong extends FastUtilMap[Long, Long] {
    type MapType = Long2LongOpenHashMap

    def createMap(initialCapacity: Int, loadFactor: Float) = {
      new Long2LongOpenHashMap(initialCapacity, loadFactor)
    }

    def trim(map: MapType) {
      map.trim()
    }
  }

  implicit object LongInt extends FastUtilMap[Long, Int] {
    type MapType = Long2IntOpenHashMap

    def createMap(initialCapacity: Int, loadFactor: Float) = {
      new Long2IntOpenHashMap(initialCapacity, loadFactor)
    }

    def trim(map: MapType) {
      map.trim()
    }
  }

  implicit object LongObject extends FastUtilMap[Long, AnyRef] {
    type MapType = Long2ObjectOpenHashMap[AnyRef]

    def createMap(initialCapacity: Int, loadFactor: Float) = {
      new Long2ObjectOpenHashMap[AnyRef](initialCapacity, loadFactor)
    }

    def trim(map: MapType) {
      map.trim()
    }
  }

}
