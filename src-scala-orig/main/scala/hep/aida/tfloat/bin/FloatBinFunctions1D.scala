package hep.aida.tfloat.bin

import FloatBinFunctions1D._
//remove if not needed
import scala.collection.JavaConversions._

object FloatBinFunctions1D {

  /**
   * Little trick to allow for "aliasing", that is, renaming this class. Using
   * the aliasing you can instead write
   * <p>
   * <tt>BinFunctions F = BinFunctions.functions; <br>
   someAlgo(F.max);</tt>
   */
  val functions = new FloatBinFunctions1D()

  /**
   * Function that returns <tt>bin.max()</tt>.
   */
  val max = new FloatBinFunction1D() {

    def apply(bin: DynamicFloatBin1D): Float = bin.max()

    def name(): String = "Max"
  }

  /**
   * Function that returns <tt>bin.mean()</tt>.
   */
  val mean = new FloatBinFunction1D() {

    def apply(bin: DynamicFloatBin1D): Float = bin.mean()

    def name(): String = "Mean"
  }

  /**
   * Function that returns <tt>bin.median()</tt>.
   */
  val median = new FloatBinFunction1D() {

    def apply(bin: DynamicFloatBin1D): Float = bin.median()

    def name(): String = "Median"
  }

  /**
   * Function that returns <tt>bin.min()</tt>.
   */
  val min = new FloatBinFunction1D() {

    def apply(bin: DynamicFloatBin1D): Float = bin.min()

    def name(): String = "Min"
  }

  /**
   * Function that returns <tt>bin.rms()</tt>.
   */
  val rms = new FloatBinFunction1D() {

    def apply(bin: DynamicFloatBin1D): Float = bin.rms()

    def name(): String = "RMS"
  }

  /**
   * Function that returns <tt>bin.size()</tt>.
   */
  val size = new FloatBinFunction1D() {

    def apply(bin: DynamicFloatBin1D): Float = bin.size

    def name(): String = "Size"
  }

  /**
   * Function that returns <tt>bin.standardDeviation()</tt>.
   */
  val stdDev = new FloatBinFunction1D() {

    def apply(bin: DynamicFloatBin1D): Float = bin.standardDeviation()

    def name(): String = "StdDev"
  }

  /**
   * Function that returns <tt>bin.sum()</tt>.
   */
  val sum = new FloatBinFunction1D() {

    def apply(bin: DynamicFloatBin1D): Float = bin.sum()

    def name(): String = "Sum"
  }

  /**
   * Function that returns <tt>bin.sumOfLogarithms()</tt>.
   */
  val sumLog = new FloatBinFunction1D() {

    def apply(bin: DynamicFloatBin1D): Float = bin.sumOfLogarithms()

    def name(): String = "SumLog"
  }

  /**
   * Function that returns <tt>bin.geometricMean()</tt>.
   */
  val geometricMean = new FloatBinFunction1D() {

    def apply(bin: DynamicFloatBin1D): Float = bin.geometricMean()

    def name(): String = "GeomMean"
  }

  /**
   * Function that returns <tt>bin.quantile(percentage)</tt>.
   *
   * @param percentage
   *            the percentage of the quantile (<tt>0 <= percentage <= 1</tt>
   *            ).
   */
  def quantile(percentage: Float): FloatBinFunction1D = {
    new FloatBinFunction1D() {

      def apply(bin: DynamicFloatBin1D): Float = bin.quantile(percentage)

      def name(): String = {
        new cern.colt.matrix.FormatterFactory().create("%1.2G")
          .form(percentage * 100) +
          "% Q."
      }
    }
  }
}

/**
 * Function objects computing dynamic bin aggregations; to be passed to generic
 * methods.
 *
 * @see cern.colt.matrix.tfloat.algo.FloatFormatter
 * @see cern.colt.matrix.tfloat.algo.FloatStatistic
 * @author wolfgang.hoschek@cern.ch
 * @version 1.0, 09/24/99
 */
class FloatBinFunctions1D protected () extends AnyRef
