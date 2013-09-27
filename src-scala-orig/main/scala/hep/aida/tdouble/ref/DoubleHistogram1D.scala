package hep.aida.tdouble.ref

import hep.aida.tdouble.DoubleIAxis
import hep.aida.tdouble.DoubleIHistogram1D
import java.util.concurrent.Future
import edu.emory.mathcs.utils.ConcurrencyUtils
//remove if not needed
import scala.collection.JavaConversions._

/**
 * A reference implementation of hep.aida.IHistogram1D. The goal is to provide a
 * clear implementation rather than the most efficient implementation. However,
 * performance seems fine - filling 1.2 * 10^6 points/sec, both using FixedAxis
 * or VariableAxis.
 *
 * @author Wolfgang Hoschek, Tony Johnson, and others.
 * @author Piotr Wendykier (piotr.wendykier@gmail.com)
 */
@SerialVersionUID(1L)
class DoubleHistogram1D(title: String, axis: DoubleIAxis) extends DoubleAbstractHistogram1D(title) with DoubleIHistogram1D {

  private var errors: Array[Double] = new Array[Double](bins + 2)

  private var heights: Array[Double] = new Array[Double](bins + 2)

  private var entries: Array[Int] = new Array[Int](bins + 2)

  private var nEntry: Int = _

  private var sumWeight: Double = _

  private var sumWeightSquared: Double = _

  private var mean: Double = _

  private var rms: Double = _

  xAxis = axis

  val bins = axis.bins()

  /**
   * Creates a variable-width histogram. Example:
   * <tt>edges = (0.2, 1.0, 5.0)</tt> yields an axis with 2 in-range bins
   * <tt>[0.2,1.0), [1.0,5.0)</tt> and 2 extra bins
   * <tt>[-inf,0.2), [5.0,inf]</tt>.
   *
   * @param title
   *            The histogram title.
   * @param edges
   *            the bin boundaries the axis shall have; must be sorted
   *            ascending and must not contain multiple identical elements.
   * @throws IllegalArgumentException
   *             if <tt>edges.length < 1</tt>.
   */
  def this(title: String, edges: Array[Double]) {
    this(title, new DoubleVariableAxis(edges))
  }

  /**
   * Creates a fixed-width histogram.
   *
   * @param title
   *            The histogram title.
   * @param bins
   *            The number of bins.
   * @param min
   *            The minimum value on the X axis.
   * @param max
   *            The maximum value on the X axis.
   */
  def this(title: String, 
      bins: Int, 
      min: Double, 
      max: Double) {
    this(title, new DoubleFixedAxis(bins, min, max))
  }

  def allEntries(): Int = nEntry

  def binEntries(index: Int): Int = entries(map(index))

  def binError(index: Int): Double = Math.sqrt(errors(map(index)))

  def binHeight(index: Int): Double = heights(map(index))

  def equivalentBinEntries(): Double = {
    sumWeight * sumWeight / sumWeightSquared
  }

  def fill(x: Double) {
    val bin = map(xAxis.coordToIndex(x))
    entries(bin) += 1
    heights(bin) += 1
    errors(bin) += 1
    nEntry += 1
    sumWeight += 1
    sumWeightSquared += 1
    mean += x
    rms += x * x
  }

  def fill(x: Double, weight: Double) {
    val bin = map(xAxis.coordToIndex(x))
    entries(bin) += 1
    heights(bin) += weight
    errors(bin) += weight * weight
    nEntry += 1
    sumWeight += weight
    sumWeightSquared += weight * weight
    mean += x * weight
    rms += x * weight * weight
  }

  def fill_2D(data: Array[Double], 
      rows: Int, 
      columns: Int, 
      zero: Int, 
      rowStride: Int, 
      columnStride: Int) {
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && 
      (rows * columns >= ConcurrencyUtils.getThreadsBeginN_1D)) {
      nthreads = Math.min(nthreads, rows)
      val futures = Array.ofDim[Future](nthreads)
      val k = rows / nthreads
      for (j <- 0 until nthreads) {
        val firstRow = j * k
        val lastRow = if ((j == nthreads - 1)) rows else firstRow + k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            var errors_loc = Array.ofDim[Double](errors.length)
            var heights_loc = Array.ofDim[Double](heights.length)
            var entries_loc = Array.ofDim[Int](entries.length)
            var nEntry_loc = 0
            var sumWeight_loc = 0
            var sumWeightSquared_loc = 0
            var mean_loc = 0
            var rms_loc = 0
            var idx = zero + firstRow * rowStride
            for (r <- firstRow until lastRow) {
              for (i <- idx until columns) {
                var bin = map(xAxis.coordToIndex(data(i)))
                entries_loc(bin) += 1
                heights_loc(bin) += 1
                errors_loc(bin) += 1
                nEntry_loc += 1
                sumWeight_loc += 1
                sumWeightSquared_loc += 1
                mean_loc += data(i)
                rms_loc += data(i) * data(i)
                i += columnStride
              }
              idx += rowStride
            }
            synchronized (this) {
              for (i <- 0 until entries.length) {
                errors(i) += errors_loc(i)
                heights(i) += heights_loc(i)
                entries(i) += entries_loc(i)
              }
              nEntry += nEntry_loc
              sumWeight += sumWeight_loc
              sumWeightSquared += sumWeightSquared_loc
              mean += mean_loc
              rms += rms_loc
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
    } else {
      var idx = zero
      for (r <- 0 until rows) {
        for (i <- idx until columns) {
          val bin = map(xAxis.coordToIndex(data(i)))
          entries(bin) += 1
          heights(bin) += 1
          errors(bin) += 1
          nEntry += 1
          sumWeight += 1
          sumWeightSquared += 1
          mean += data(i)
          rms += data(i) * data(i)
          i += columnStride
        }
        idx += rowStride
      }
    }
  }

  def fill_2D(data: Array[Double], 
      weights: Array[Double], 
      rows: Int, 
      columns: Int, 
      zero: Int, 
      rowStride: Int, 
      columnStride: Int) {
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && 
      (rows * columns >= ConcurrencyUtils.getThreadsBeginN_1D)) {
      nthreads = Math.min(nthreads, rows)
      val futures = Array.ofDim[Future](nthreads)
      val k = rows / nthreads
      for (j <- 0 until nthreads) {
        val firstRow = j * k
        val lastRow = if ((j == nthreads - 1)) rows else firstRow + k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            var idx = zero + firstRow * rowStride
            var errors_loc = Array.ofDim[Double](errors.length)
            var heights_loc = Array.ofDim[Double](heights.length)
            var entries_loc = Array.ofDim[Int](entries.length)
            var nEntry_loc = 0
            var sumWeight_loc = 0
            var sumWeightSquared_loc = 0
            var mean_loc = 0
            var rms_loc = 0
            for (r <- firstRow until lastRow) {
              for (i <- idx until columns) {
                var bin = map(xAxis.coordToIndex(data(i)))
                var widx = r * columns + c
                var w2 = weights(widx) * weights(widx)
                entries_loc(bin) += 1
                heights_loc(bin) += weights(widx)
                errors_loc(bin) += w2
                nEntry_loc += 1
                sumWeight_loc += weights(widx)
                sumWeightSquared_loc += w2
                mean_loc += data(i) * weights(widx)
                rms_loc += data(i) * w2
                i += columnStride
              }
              idx += rowStride
            }
            synchronized (this) {
              for (i <- 0 until entries.length) {
                errors(i) += errors_loc(i)
                heights(i) += heights_loc(i)
                entries(i) += entries_loc(i)
              }
              nEntry += nEntry_loc
              sumWeight += sumWeight_loc
              sumWeightSquared += sumWeightSquared_loc
              mean += mean_loc
              rms += rms_loc
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
    } else {
      var idx = zero
      for (r <- 0 until rows) {
        for (i <- idx until columns) {
          val bin = map(xAxis.coordToIndex(data(i)))
          val widx = r * columns + c
          val w2 = weights(widx) * weights(widx)
          entries(bin) += 1
          heights(bin) += weights(widx)
          errors(bin) += w2
          nEntry += 1
          sumWeight += weights(widx)
          sumWeightSquared += w2
          mean += data(i) * weights(widx)
          rms += data(i) * w2
          i += columnStride
        }
        idx += rowStride
      }
    }
  }

  /**
   * Returns the contents of this histogram.
   *
   * @return the contents of this histogram
   */
  def getContents(): DoubleHistogram1DContents = {
    new DoubleHistogram1DContents(entries, heights, errors, nEntry, sumWeight, sumWeightSquared, mean, 
      rms)
  }

  def mean(): Double = mean / sumWeight

  def reset() {
    for (i <- 0 until entries.length) {
      entries(i) = 0
      heights(i) = 0
      errors(i) = 0
    }
    nEntry = 0
    sumWeight = 0
    sumWeightSquared = 0
    mean = 0
    rms = 0
  }

  def rms(): Double = {
    Math.sqrt(rms / sumWeight - mean * mean / sumWeight / sumWeight)
  }

  /**
   * Sets the contents of this histogram.
   *
   * @param contents
   */
  def setContents(contents: DoubleHistogram1DContents) {
    this.entries = contents.getEntries
    this.heights = contents.getHeights
    this.errors = contents.getErrors
    this.nEntry = contents.getNentry
    this.sumWeight = contents.getSumWeight
    this.sumWeightSquared = contents.getSumWeightSquared
    this.mean = contents.getMean
    this.rms = contents.getRms
  }

  /**
   * Used internally for creating slices and projections
   */
  def setContents(entries: Array[Int], heights: Array[Double], errors: Array[Double]) {
    this.entries = entries
    this.heights = heights
    this.errors = errors
    for (i <- 0 until entries.length) {
      nEntry += entries(i)
      sumWeight += heights(i)
    }
    sumWeightSquared = Double.NaN
    mean = Double.NaN
    rms = Double.NaN
  }
}
