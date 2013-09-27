package hep.aida.tdouble.bin

import DoubleBinFunctions1D._
//remove if not needed
import scala.collection.JavaConversions._

object DoubleBinFunctions1D {

  /**
   * Little trick to allow for "aliasing", that is, renaming this class. Using
   * the aliasing you can instead write
   * <p>
   * <tt>BinFunctions F = BinFunctions.functions; <br>
   someAlgo(F.max);</tt>
   */
  val functions = new DoubleBinFunctions1D()

  /**
   * Function that returns <tt>bin.max()</tt>.
   */
  val max = new DoubleBinFunction1D() {

    def apply(bin: DynamicDoubleBin1D): Double = bin.max()

    def name(): String = "Max"
  }

  /**
   * Function that returns <tt>bin.mean()</tt>.
   */
  val mean = new DoubleBinFunction1D() {

    def apply(bin: DynamicDoubleBin1D): Double = bin.mean()

    def name(): String = "Mean"
  }

  /**
   * Function that returns <tt>bin.median()</tt>.
   */
  val median = new DoubleBinFunction1D() {

    def apply(bin: DynamicDoubleBin1D): Double = bin.median()

    def name(): String = "Median"
  }

  /**
   * Function that returns <tt>bin.min()</tt>.
   */
  val min = new DoubleBinFunction1D() {

    def apply(bin: DynamicDoubleBin1D): Double = bin.min()

    def name(): String = "Min"
  }

  /**
   * Function that returns <tt>bin.rms()</tt>.
   */
  val rms = new DoubleBinFunction1D() {

    def apply(bin: DynamicDoubleBin1D): Double = bin.rms()

    def name(): String = "RMS"
  }

  /**
   * Function that returns <tt>bin.size()</tt>.
   */
  val size = new DoubleBinFunction1D() {

    def apply(bin: DynamicDoubleBin1D): Double = bin.size

    def name(): String = "Size"
  }

  /**
   * Function that returns <tt>bin.standardDeviation()</tt>.
   */
  val stdDev = new DoubleBinFunction1D() {

    def apply(bin: DynamicDoubleBin1D): Double = bin.standardDeviation()

    def name(): String = "StdDev"
  }

  /**
   * Function that returns <tt>bin.sum()</tt>.
   */
  val sum = new DoubleBinFunction1D() {

    def apply(bin: DynamicDoubleBin1D): Double = bin.sum()

    def name(): String = "Sum"
  }

  /**
   * Function that returns <tt>bin.sumOfLogarithms()</tt>.
   */
  val sumLog = new DoubleBinFunction1D() {

    def apply(bin: DynamicDoubleBin1D): Double = bin.sumOfLogarithms()

    def name(): String = "SumLog"
  }

  /**
   * Function that returns <tt>bin.geometricMean()</tt>.
   */
  val geometricMean = new DoubleBinFunction1D() {

    def apply(bin: DynamicDoubleBin1D): Double = bin.geometricMean()

    def name(): String = "GeomMean"
  }

  /**
   * Function that returns <tt>bin.quantile(percentage)</tt>.
   *
   * @param percentage
   *            the percentage of the quantile (<tt>0 <= percentage <= 1</tt>
   *            ).
   */
  def quantile(percentage: Double): DoubleBinFunction1D = {
    new DoubleBinFunction1D() {

      def apply(bin: DynamicDoubleBin1D): Double = bin.quantile(percentage)

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
 * @see cern.colt.matrix.tdouble.algo.DoubleFormatter
 * @see cern.colt.matrix.tdouble.algo.DoubleStatistic
 * @author wolfgang.hoschek@cern.ch
 * @version 1.0, 09/24/99
 */
class DoubleBinFunctions1D protected () extends AnyRef
