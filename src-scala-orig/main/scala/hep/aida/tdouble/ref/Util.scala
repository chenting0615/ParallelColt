package hep.aida.tdouble.ref

import hep.aida.tdouble.DoubleIHistogram1D
import hep.aida.tdouble.DoubleIHistogram2D
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Convenient histogram utilities.
 */
class Util {

  /**
   * Returns the index of the in-range bin containing the maxBinHeight().
   */
  def maxBin(h: DoubleIHistogram1D): Int = {
    var maxBin = -1
    var maxValue = Double.MIN_VALUE
    var i = h.xAxis().bins()
    while (i >= 0) {
      val value = h.binHeight(i)
      if (value > maxValue) {
        maxValue = value
        maxBin = i
      }
    }
    maxBin
  }

  /**
   * Returns the indexX of the in-range bin containing the maxBinHeight().
   */
  def maxBinX(h: DoubleIHistogram2D): Int = {
    var maxValue = Double.MIN_VALUE
    var maxBinX = -1
    var maxBinY = -1
    var i = h.xAxis().bins()
    while (i >= 0) {
      var j = h.yAxis().bins()
      while (j >= 0) {
        val value = h.binHeight(i, j)
        if (value > maxValue) {
          maxValue = value
          maxBinX = i
          maxBinY = j
        }
      }
    }
    maxBinX
  }

  /**
   * Returns the indexY of the in-range bin containing the maxBinHeight().
   */
  def maxBinY(h: DoubleIHistogram2D): Int = {
    var maxValue = Double.MIN_VALUE
    var maxBinX = -1
    var maxBinY = -1
    var i = h.xAxis().bins()
    while (i >= 0) {
      var j = h.yAxis().bins()
      while (j >= 0) {
        val value = h.binHeight(i, j)
        if (value > maxValue) {
          maxValue = value
          maxBinX = i
          maxBinY = j
        }
      }
    }
    maxBinY
  }

  /**
   * Returns the index of the in-range bin containing the minBinHeight().
   */
  def minBin(h: DoubleIHistogram1D): Int = {
    var minBin = -1
    var minValue = Double.MAX_VALUE
    var i = h.xAxis().bins()
    while (i >= 0) {
      val value = h.binHeight(i)
      if (value < minValue) {
        minValue = value
        minBin = i
      }
    }
    minBin
  }

  /**
   * Returns the indexX of the in-range bin containing the minBinHeight().
   */
  def minBinX(h: DoubleIHistogram2D): Int = {
    var minValue = Double.MAX_VALUE
    var minBinX = -1
    var minBinY = -1
    var i = h.xAxis().bins()
    while (i >= 0) {
      var j = h.yAxis().bins()
      while (j >= 0) {
        val value = h.binHeight(i, j)
        if (value < minValue) {
          minValue = value
          minBinX = i
          minBinY = j
        }
      }
    }
    minBinX
  }

  /**
   * Returns the indexY of the in-range bin containing the minBinHeight().
   */
  def minBinY(h: DoubleIHistogram2D): Int = {
    var minValue = Double.MAX_VALUE
    var minBinX = -1
    var minBinY = -1
    var i = h.xAxis().bins()
    while (i >= 0) {
      var j = h.yAxis().bins()
      while (j >= 0) {
        val value = h.binHeight(i, j)
        if (value < minValue) {
          minValue = value
          minBinX = i
          minBinY = j
        }
      }
    }
    minBinY
  }
}
