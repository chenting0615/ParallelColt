package cern.colt

import java.util.Comparator
import java.util.concurrent.ExecutionException
import java.util.concurrent.Future
import cern.colt.function.tbyte.ByteComparator
import cern.colt.function.tchar.CharComparator
import cern.colt.function.tdouble.PrimitiveComparator
import cern.colt.function.tfloat.FloatComparator
import cern.colt.function.tint.IntComparator
import cern.colt.function.tlong.LongComparator
import cern.colt.function.tshort.ShortComparator
import edu.emory.mathcs.utils.ConcurrencyUtils

object ParallelQuickSort {

  private val SMALL = 7

  private val MEDIUM = 40

  /**
   *
   * Multithreaded quicksort.
   *
   * @param x
   *            array to be sorted
   * @param off
   *            first index of subarray
   * @param len
   *            length of subarray
   * @param comp
   *            comparator
   * @param nThreads
   *            number of threads
   */
  def quickSort(x: Array[Byte],
      off: Int,
      len: Int,
      comp: ByteComparator,
      nThreads: Int) {
    if (len < SMALL) {
      for (i <- off until len + off) {
        var j = i
        while (j > off && comp.compare(x(j - 1), x(j)) > 0) {
          swap(x, j, j - 1)
          j -= 1
        }
      }
      return
    }
    var m = off + len / 2
    if (len > SMALL) {
      var l = off
      var n = off + len - 1
      if (len > MEDIUM) {
        val s = len / 8
        l = med3(x, l, l + s, l + 2 * s, comp)
        m = med3(x, m - s, m, m + s, comp)
        n = med3(x, n - 2 * s, n - s, n, comp)
      }
      m = med3(x, l, m, n, comp)
    }
    val v = x(m)
    var a = off
    var b = a
    var c = off + len - 1
    var d = c
    var keepGoing = true
    while (keepGoing) {
      var comparison: Int = 0
      comparison = comp.compare(x(b), v)
      while (b <= c && comparison <= 0) {
        if (comparison == 0) {swap(x, a, b); a+= 1}
        b += 1
        comparison = comp.compare(x(b), v)
      }
      comparison = comp.compare(x(c), v)
      while (c >= b && comparison >= 0) {
        if (comparison == 0) {swap(x, c, d); d -= 1}
        c -= 1
        comparison = comp.compare(x(c), v)
      }
      if (b > c)
        keepGoing = false
      else {
        swap(x, b, c)
        b += 1
        c -= 1
      }
    }
    var s: Int = 0
    var n = off + len
    s = Math.min(a - off, b - a)
    vecswap(x, off, b - s, s)
    s = Math.min(d - c, n - d - 1)
    vecswap(x, b, n - s, s)
    var other: Future[_] = null
    if (nThreads > 1) {
      if (b - a > 1) {
        s = b - a
        val s_f = s
        other = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            quickSort(x, off, s_f, comp, nThreads / 2)
          }
        })
      }
      if (d - c > 1) {
        s = d - c
        if (other == null) {
          val s_f = s
          val ns_f = n - s
          other = ConcurrencyUtils.submit(new Runnable() {

            def run() {
              quickSort(x, ns_f, s_f, comp, nThreads / 2)
            }
          })
        } else {
          quickSort(x, n - s, s, comp, nThreads / 2)
        }
      }
      try {
        other.get
      } catch {
        case e: InterruptedException => throw new RuntimeException("thread interrupted")
        case e: ExecutionException => e.printStackTrace()
      }
    } else {
      if (b - a > 1) {s = b - a; quickSort(x, off, s, comp, 1)}
      if (d - c > 1) {s = d - c; quickSort(x, n - s, s, comp, 1)}
    }
  }

  /**
   *
   * Multithreaded quicksort.
   *
   * @param x
   *            array to be sorted
   * @param off
   *            first index of subarray
   * @param len
   *            length of subarray
   * @param comp
   *            comparator
   * @param nThreads
   *            number of threads
   */
  def quickSort(x: Array[Char],
      off: Int,
      len: Int,
      comp: CharComparator,
      nThreads: Int) {
    if (len < SMALL) {
      for (i <- off until len + off) {
        var j = i
        while (j > off && comp.compare(x(j - 1), x(j)) > 0) {
          swap(x, j, j - 1)
          j -= 1
        }
      }
      return
    }
    var m = off + len / 2
    if (len > SMALL) {
      var l = off
      var n = off + len - 1
      if (len > MEDIUM) {
        val s = len / 8
        l = med3(x, l, l + s, l + 2 * s, comp)
        m = med3(x, m - s, m, m + s, comp)
        n = med3(x, n - 2 * s, n - s, n, comp)
      }
      m = med3(x, l, m, n, comp)
    }
    val v = x(m)
    var a = off
    var b = a
    var c = off + len - 1
    var d = c
    var keepGoing = true
    while (keepGoing) {
      var comparison: Int = 0
      comparison = comp.compare(x(b), v)
      while (b <= c && comparison <= 0) {
        if (comparison == 0) {swap(x, a, b); a += 1}
        b += 1
        comparison = comp.compare(x(b), v)
      }
      comparison = comp.compare(x(c), v)
      while (c >= b && comparison >= 0) {
        if (comparison == 0) {swap(x, c, d); d -= 1}
        c -= 1
        comparison = comp.compare(x(c), v)
      }
      if (b > c)
        keepGoing = false
      else {
        swap(x, b, c)
        b += 1
        c -= 1
      }
    }
    var s: Int = 0
    var n = off + len
    s = Math.min(a - off, b - a)
    vecswap(x, off, b - s, s)
    s = Math.min(d - c, n - d - 1)
    vecswap(x, b, n - s, s)
    var other: Future[_] = null
    if (nThreads > 1) {
      s = b - a
      if (s > 1) {
        val s_f = s
        other = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            quickSort(x, off, s_f, comp, nThreads / 2)
          }
        })
      }
      s = d - c
      if (s > 1) {
        if (other == null) {
          val s_f = s
          val ns_f = n - s
          other = ConcurrencyUtils.submit(new Runnable() {

            def run() {
              quickSort(x, ns_f, s_f, comp, nThreads / 2)
            }
          })
        } else {
          quickSort(x, n - s, s, comp, nThreads / 2)
        }
      }
      try {
        other.get
      } catch {
        case e: InterruptedException => throw new RuntimeException("thread interrupted")
        case e: ExecutionException => e.printStackTrace()
      }
    } else {
      s = b - a
      if (s > 1) quickSort(x, off, s, comp, 1)
      s = d - c
      if (s > 1) quickSort(x, n - s, s, comp, 1)
    }
  }

  /**
   *
   * Multithreaded quicksort.
   *
   * @param x
   *            array to be sorted
   * @param off
   *            first index of subarray
   * @param len
   *            length of subarray
   * @param comp
   *            comparator
   * @param nThreads
   *            number of threads
   */
  def quickSort(x: Array[Double],
      off: Int,
      len: Int,
      comp: PrimitiveComparator,
      nThreads: Int) {
    if (len < SMALL) {
      for (i <- off until len + off) {
        var j = i
        while (j > off && comp.compare(x(j - 1), x(j)) > 0) {
          swap(x, j, j - 1)
          j -= 1
        }
      }
      return
    }
    var m = off + len / 2
    if (len > SMALL) {
      var l = off
      var n = off + len - 1
      if (len > MEDIUM) {
        val s = len / 8
        l = med3(x, l, l + s, l + 2 * s, comp)
        m = med3(x, m - s, m, m + s, comp)
        n = med3(x, n - 2 * s, n - s, n, comp)
      }
      m = med3(x, l, m, n, comp)
    }
    val v = x(m)
    var a = off
    var b = a
    var c = off + len - 1
    var d = c
    var keepGoing = true
    while (keepGoing) {
      var comparison: Int = 0
      comparison = comp.compare(x(b), v)
      while (b <= c && comparison <= 0) {
        if (comparison == 0) {swap(x, a, b); a += 1}
        b += 1
        comparison = comp.compare(x(b), v)
      }
      comparison = comp.compare(x(c), v)
      while (c >= b && comparison >= 0) {
        if (comparison == 0) {swap(x, c, d); d -= 1}
        c -= 1
        comparison = comp.compare(x(c), v)
      }
      if (b > c)
        keepGoing = false
      else {
        swap(x, b, c)
        b += 1
        c -= 1
      }
    }
    var s: Int = 0
    var n = off + len
    s = Math.min(a - off, b - a)
    vecswap(x, off, b - s, s)
    s = Math.min(d - c, n - d - 1)
    vecswap(x, b, n - s, s)
    var other: Future[_] = null
    if (nThreads > 1) {
      s = b - a
      if (s > 1) {
        val s_f = s
        other = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            quickSort(x, off, s_f, comp, nThreads / 2)
          }
        })
      }
      s = d - c
      if (s > 1) {
        if (other == null) {
          val s_f = s
          val ns_f = n - s
          other = ConcurrencyUtils.submit(new Runnable() {

            def run() {
              quickSort(x, ns_f, s_f, comp, nThreads / 2)
            }
          })
        } else {
          quickSort(x, n - s, s, comp, nThreads / 2)
        }
      }
      try {
        other.get
      } catch {
        case e: InterruptedException => throw new RuntimeException("thread interrupted")
        case e: ExecutionException => e.printStackTrace()
      }
    } else {
      s = b - a
      if (s > 1) quickSort(x, off, s, comp, 1)
      s = d - c
      if (s > 1) quickSort(x, n - s, s, comp, 1)
    }
  }

  /**
   *
   * Multithreaded quicksort.
   *
   * @param x
   *            array to be sorted
   * @param off
   *            first index of subarray
   * @param len
   *            length of subarray
   * @param comp
   *            comparator
   * @param nThreads
   *            number of threads
   */
  def quickSort(x: Array[Float],
      off: Int,
      len: Int,
      comp: FloatComparator,
      nThreads: Int) {
    if (len < SMALL) {
      for (i <- off until len + off) {
        var j = i
        while (j > off && comp.compare(x(j - 1), x(j)) > 0) {
          swap(x, j, j - 1)
          j -= 1
        }
      }
      return
    }
    var m = off + len / 2
    if (len > SMALL) {
      var l = off
      var n = off + len - 1
      if (len > MEDIUM) {
        val s = len / 8
        l = med3(x, l, l + s, l + 2 * s, comp)
        m = med3(x, m - s, m, m + s, comp)
        n = med3(x, n - 2 * s, n - s, n, comp)
      }
      m = med3(x, l, m, n, comp)
    }
    val v = x(m)
    var a = off
    var b = a
    var c = off + len - 1
    var d = c
    var keepGoing = true
    while (keepGoing) {
      var comparison: Int = 0
      comparison = comp.compare(x(b), v)
      while (b <= c && comparison <= 0) {
        if (comparison == 0) {swap(x, a, b); a += 1}
        b += 1
        comparison = comp.compare(x(b), v)
      }
      comparison = comp.compare(x(c), v)
      while (c >= b && comparison >= 0) {
        if (comparison == 0) {swap(x, c, d); d -= 1}
        c -= 1
        comparison = comp.compare(x(c), v)
      }
      if (b > c)
        keepGoing = false
      else {
        swap(x, b, c)
        b += 1
        c -= 1
      }
    }
    var s: Int = 0
    var n = off + len
    s = Math.min(a - off, b - a)
    vecswap(x, off, b - s, s)
    s = Math.min(d - c, n - d - 1)
    vecswap(x, b, n - s, s)
    var other: Future[_] = null
    if (nThreads > 1) {
      s = b - a
      if (s > 1) {
        val s_f = s
        other = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            quickSort(x, off, s_f, comp, nThreads / 2)
          }
        })
      }
      s = d - c
      if (s > 1) {
        if (other == null) {
          val s_f = s
          val ns_f = n - s
          other = ConcurrencyUtils.submit(new Runnable() {

            def run() {
              quickSort(x, ns_f, s_f, comp, nThreads / 2)
            }
          })
        } else {
          quickSort(x, n - s, s, comp, nThreads / 2)
        }
      }
      try {
        other.get
      } catch {
        case e: InterruptedException => throw new RuntimeException("thread interrupted")
        case e: ExecutionException => e.printStackTrace()
      }
    } else {
      s = b - a
      if (s > 1) quickSort(x, off, s, comp, 1)
      s = d - c
      if (s > 1) quickSort(x, n - s, s, comp, 1)
    }
  }

  /**
   *
   * Multithreaded quicksort.
   *
   * @param x
   *            array to be sorted
   * @param off
   *            first index of subarray
   * @param len
   *            length of subarray
   * @param comp
   *            comparator
   * @param nThreads
   *            number of threads
   */
  def quickSort(x: Array[Int],
      off: Int,
      len: Int,
      comp: IntComparator,
      nThreads: Int) {
    if (len < SMALL) {
      for (i <- off until len + off) {
        var j = i
        while (j > off && comp.compare(x(j - 1), x(j)) > 0) {
          swap(x, j, j - 1)
          j -= 1
        }
      }
      return
    }
    var m = off + len / 2
    if (len > SMALL) {
      var l = off
      var n = off + len - 1
      if (len > MEDIUM) {
        val s = len / 8
        l = med3(x, l, l + s, l + 2 * s, comp)
        m = med3(x, m - s, m, m + s, comp)
        n = med3(x, n - 2 * s, n - s, n, comp)
      }
      m = med3(x, l, m, n, comp)
    }
    val v = x(m)
    var a = off
    var b = a
    var c = off + len - 1
    var d = c
    var keepGoing = true
    while (keepGoing) {
      var comparison: Int = 0
      comparison = comp.compare(x(b), v)
      while (b <= c && comparison <= 0) {
        if (comparison == 0) {swap(x, a, b); a += 1}
        b += 1
        comparison = comp.compare(x(b), v)
      }
      comparison = comp.compare(x(c), v)
      while (c >= b && comparison >= 0) {
        if (comparison == 0) {swap(x, c, d); d -= 1}
        c -= 1
        comparison = comp.compare(x(c), v)
      }
      if (b > c)
        keepGoing = false
      else {
        swap(x, b, c)
        b += 1
        c -= 1
      }
    }
    var s: Int = 0
    var n = off + len
    s = Math.min(a - off, b - a)
    vecswap(x, off, b - s, s)
    s = Math.min(d - c, n - d - 1)
    vecswap(x, b, n - s, s)
    var other: Future[_] = null
    if (nThreads > 1) {
      s = b - a
      if (s > 1) {
        val s_f = s
        other = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            quickSort(x, off, s_f, comp, nThreads / 2)
          }
        })
      }
      s = d - c
      if (s > 1) {
        if (other == null) {
          val s_f = s
          val ns_f = n - s
          other = ConcurrencyUtils.submit(new Runnable() {

            def run() {
              quickSort(x, ns_f, s_f, comp, nThreads / 2)
            }
          })
        } else {
          quickSort(x, n - s, s, comp, nThreads / 2)
        }
      }
      try {
        other.get
      } catch {
        case e: InterruptedException => throw new RuntimeException("thread interrupted")
        case e: ExecutionException => e.printStackTrace()
      }
    } else {
      s = b - a
      if (s > 1) quickSort(x, off, s, comp, 1)
      s = d - c
      if (s > 1) quickSort(x, n - s, s, comp, 1)
    }
  }

  /**
   *
   * Multithreaded quicksort.
   *
   * @param x
   *            array to be sorted
   * @param off
   *            first index of subarray
   * @param len
   *            length of subarray
   * @param comp
   *            comparator
   * @param nThreads
   *            number of threads
   */
  def quickSort(x: Array[Long],
      off: Int,
      len: Int,
      comp: LongComparator,
      nThreads: Int) {
    if (len < SMALL) {
      for (i <- off until len + off) {
        var j = i
        while (j > off && comp.compare(x(j - 1), x(j)) > 0) {
          swap(x, j, j - 1)
          j -= 1
        }
      }
      return
    }
    var m = off + len / 2
    if (len > SMALL) {
      var l = off
      var n = off + len - 1
      if (len > MEDIUM) {
        val s = len / 8
        l = med3(x, l, l + s, l + 2 * s, comp)
        m = med3(x, m - s, m, m + s, comp)
        n = med3(x, n - 2 * s, n - s, n, comp)
      }
      m = med3(x, l, m, n, comp)
    }
    val v = x(m)
    var a = off
    var b = a
    var c = off + len - 1
    var d = c
    var keepGoing = true
    while (keepGoing) {
      var comparison: Int = 0
      comparison = comp.compare(x(b), v)
      while (b <= c && comparison <= 0) {
        if (comparison == 0) {swap(x, a, b); a += 1}
        b += 1
        comparison = comp.compare(x(b), v)
      }
      comparison = comp.compare(x(c), v)
      while (c >= b && comparison >= 0) {
        if (comparison == 0) {swap(x, c, d); d -= 1}
        c -= 1
        comparison = comp.compare(x(c), v)
      }
      if (b > c)
        keepGoing = false
      else {
        swap(x, b, c)
        b += 1
        c -= 1
      }
    }
    var s: Int = 0
    var n = off + len
    s = Math.min(a - off, b - a)
    vecswap(x, off, b - s, s)
    s = Math.min(d - c, n - d - 1)
    vecswap(x, b, n - s, s)
    var other: Future[_] = null
    if (nThreads > 1) {
      s = b - a
      if (s > 1) {
        val s_f = s
        other = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            quickSort(x, off, s_f, comp, nThreads / 2)
          }
        })
      }
      s = d - c
      if (s > 1) {
        if (other == null) {
          val s_f = s
          val ns_f = n - s
          other = ConcurrencyUtils.submit(new Runnable() {

            def run() {
              quickSort(x, ns_f, s_f, comp, nThreads / 2)
            }
          })
        } else {
          quickSort(x, n - s, s, comp, nThreads / 2)
        }
      }
      try {
        other.get
      } catch {
        case e: InterruptedException => throw new RuntimeException("thread interrupted")
        case e: ExecutionException => e.printStackTrace()
      }
    } else {
      s = b - a
      if (s > 1) quickSort(x, off, s, comp, 1)
      s = d - c
      if (s > 1) quickSort(x, n - s, s, comp, 1)
    }
  }

  /**
   *
   * Multithreaded quicksort.
   *
   * @param x
   *            array to be sorted
   * @param off
   *            first index of subarray
   * @param len
   *            length of subarray
   * @param nThreads
   *            number of threads
   */
  def quickSort[T <: Comparable[T]](x: Array[T],
      off: Int,
      len: Int,
      nThreads: Int) {
    if (len < SMALL) {
      for (i <- off until len + off) {
        var j = i
        while (j > off && x(j - 1).compareTo(x(j)) > 0) {
          swap(x, j, j - 1)
          j -= 1
        }
      }
      return
    }
    var m = off + len / 2
    if (len > SMALL) {
      var l = off
      var n = off + len - 1
      if (len > MEDIUM) {
        val s = len / 8
        l = med3(x, l, l + s, l + 2 * s)
        m = med3(x, m - s, m, m + s)
        n = med3(x, n - 2 * s, n - s, n)
      }
      m = med3(x, l, m, n)
    }
    val v = x(m)
    var a = off
    var b = a
    var c = off + len - 1
    var d = c
    var keepGoing = true
    while (keepGoing) {
      var comparison: Int = 0
      comparison = x(b).compareTo(v)
      while (b <= c && comparison <= 0) {
        if (comparison == 0) {swap(x, a, b); a += 1}
        b += 1
        comparison = x(b).compareTo(v)
      }
      comparison = x(c).compareTo(v)
      while (c >= b && comparison >= 0) {
        if (comparison == 0) {swap(x, c, d); d -= 1}
        c -= 1
        comparison = x(c).compareTo(v)
      }
      if (b > c)
        keepGoing = false
      else {
        swap(x, b, c)
        b += 1
        c -= 1
      }
    }
    var s: Int = 0
    var n = off + len
    s = Math.min(a - off, b - a)
    vecswap(x, off, b - s, s)
    s = Math.min(d - c, n - d - 1)
    vecswap(x, b, n - s, s)
    var other: Future[_] = null
    if (nThreads > 1) {
      s = b - a
      if (s > 1) {
        val s_f = s
        other = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            quickSort(x, off, s_f, nThreads / 2)
          }
        })
      }
      s = d - c
      if (s > 1) {
        if (other == null) {
          val s_f = s
          val ns_f = n - s
          other = ConcurrencyUtils.submit(new Runnable() {

            def run() {
              quickSort(x, ns_f, s_f, nThreads / 2)
            }
          })
        } else {
          quickSort(x, n - s, s, nThreads / 2)
        }
      }
      try {
        other.get
      } catch {
        case e: InterruptedException => throw new RuntimeException("thread interrupted")
        case e: ExecutionException => e.printStackTrace()
      }
    } else {
      s = b - a
      if (s > 1) quickSort(x, off, s, 1)
      s = d - c
      if (s > 1) quickSort(x, n - s, s, 1)
    }
  }

  /**
   *
   * Multithreaded quicksort.
   *
   * @param x
   *            array to be sorted
   * @param off
   *            first index of subarray
   * @param len
   *            length of subarray
   * @param comp
   *            comparator
   * @param nThreads
   *            number of threads
   */
  def quickSort[T](x: Array[T],
      off: Int,
      len: Int,
      comp: Comparator[T],
      nThreads: Int) {
    if (len < SMALL) {
      for (i <- off until len + off) {
        var j = i
        while (j > off && comp.compare(x(j - 1), x(j)) > 0) {
          swap(x, j, j - 1)
          j -= 1
        }
      }
      return
    }
    var m = off + len / 2
    if (len > SMALL) {
      var l = off
      var n = off + len - 1
      if (len > MEDIUM) {
        val s = len / 8
        l = med3(x, l, l + s, l + 2 * s, comp)
        m = med3(x, m - s, m, m + s, comp)
        n = med3(x, n - 2 * s, n - s, n, comp)
      }
      m = med3(x, l, m, n, comp)
    }
    val v = x(m)
    var a = off
    var b = a
    var c = off + len - 1
    var d = c
    var keepGoing = true
    while (keepGoing) {
      var comparison: Int = 0
      comparison = comp.compare(x(b), v)
      while (b <= c && comparison <= 0) {
        if (comparison == 0) {swap(x, a, b); a + 1}
        b += 1
        comparison = comp.compare(x(b), v)
      }
      comparison = comp.compare(x(c), v)
      while (c >= b && comparison >= 0) {
        if (comparison == 0) {swap(x, c, d); d -= 1}
        c -= 1
        comparison = comp.compare(x(c), v)
      }
      if (b > c)
        keepGoing = false
      else {
        swap(x, b, c)
        b += 1
        c -= 1
      }
    }
    var s: Int = 0
    var n = off + len
    s = Math.min(a - off, b - a)
    vecswap(x, off, b - s, s)
    s = Math.min(d - c, n - d - 1)
    vecswap(x, b, n - s, s)
    var other: Future[_] = null
    if (nThreads > 1) {
      s = b - a
      if (s > 1) {
        val s_f = s
        other = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            quickSort(x, off, s_f, comp, nThreads / 2)
          }
        })
      }
      s = d - c
      if (s > 1) {
        if (other == null) {
          val s_f = s
          val ns_f = n - s
          other = ConcurrencyUtils.submit(new Runnable() {

            def run() {
              quickSort(x, ns_f, s_f, comp, nThreads / 2)
            }
          })
        } else {
          quickSort(x, n - s, s, comp, nThreads / 2)
        }
      }
      try {
        other.get
      } catch {
        case e: InterruptedException => throw new RuntimeException("thread interrupted")
        case e: ExecutionException => e.printStackTrace()
      }
    } else {
      s = b - a
      if (s > 1) quickSort(x, off, s, comp, 1)
      s = d - c
      if (s > 1) quickSort(x, n - s, s, comp, 1)
    }
  }

  /**
   *
   * Multithreaded quicksort.
   *
   * @param x
   *            array to be sorted
   * @param off
   *            first index of subarray
   * @param len
   *            length of subarray
   * @param comp
   *            comparator
   * @param nThreads
   *            number of threads
   */
  def quickSort(x: Array[Short],
      off: Int,
      len: Int,
      comp: ShortComparator,
      nThreads: Int) {
    if (len < SMALL) {
      for (i <- off until len + off) {
        var j = i
        while (j > off && comp.compare(x(j - 1), x(j)) > 0) {
          swap(x, j, j - 1)
          j -= 1
        }
      }
      return
    }
    var m = off + len / 2
    if (len > SMALL) {
      var l = off
      var n = off + len - 1
      if (len > MEDIUM) {
        val s = len / 8
        l = med3(x, l, l + s, l + 2 * s, comp)
        m = med3(x, m - s, m, m + s, comp)
        n = med3(x, n - 2 * s, n - s, n, comp)
      }
      m = med3(x, l, m, n, comp)
    }
    val v = x(m)
    var a = off
    var b = a
    var c = off + len - 1
    var d = c
    var keepGoing = true
    while (keepGoing) {
      var comparison: Int = 0
      comparison = comp.compare(x(b), v)
      while (b <= c && comparison <= 0) {
        if (comparison == 0) {swap(x, a, b); a += 1}
        b += 1
        comparison = comp.compare(x(b), v)
      }
      comparison = comp.compare(x(c), v)
      while (c >= b && comparison >= 0) {
        if (comparison == 0) {swap(x, c, d); d -= 1}
        c -= 1
        comparison = comp.compare(x(c), v)
      }
      if (b > c)
        keepGoing = false
      else {
        swap(x, b, c)
        b += 1
        c -= 1
      }
    }
    var s: Int = 0
    var n = off + len
    s = Math.min(a - off, b - a)
    vecswap(x, off, b - s, s)
    s = Math.min(d - c, n - d - 1)
    vecswap(x, b, n - s, s)
    var other: Future[_] = null
    if (nThreads > 1) {
      s = b - a
      if (s > 1) {
        val s_f = s
        other = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            quickSort(x, off, s_f, comp, nThreads / 2)
          }
        })
      }
      s = d - c
      if (s > 1) {
        if (other == null) {
          val s_f = s
          val ns_f = n - s
          other = ConcurrencyUtils.submit(new Runnable() {

            def run() {
              quickSort(x, ns_f, s_f, comp, nThreads / 2)
            }
          })
        } else {
          quickSort(x, n - s, s, comp, nThreads / 2)
        }
      }
      try {
        other.get
      } catch {
        case e: InterruptedException => throw new RuntimeException("thread interrupted")
        case e: ExecutionException => e.printStackTrace()
      }
    } else {
      s = b - a
      if (s > 1) quickSort(x, off, s, comp, 1)
      s = d - c
      if (s > 1) quickSort(x, n - s, s, comp, 1)
    }
  }

  /**
   * Swaps x[a] with x[b].
   */
  private def swap(x: Array[Byte], a: Int, b: Int) {
    val t = x(a)
    x(a) = x(b)
    x(b) = t
  }

  /**
   * Swaps x[a] with x[b].
   */
  private def swap(x: Array[Char], a: Int, b: Int) {
    val t = x(a)
    x(a) = x(b)
    x(b) = t
  }

  /**
   * Swaps x[a] with x[b].
   */
  private def swap(x: Array[Double], a: Int, b: Int) {
    val t = x(a)
    x(a) = x(b)
    x(b) = t
  }

  /**
   * Swaps x[a] with x[b].
   */
  private def swap(x: Array[Float], a: Int, b: Int) {
    val t = x(a)
    x(a) = x(b)
    x(b) = t
  }

  /**
   * Swaps x[a] with x[b].
   */
  private def swap(x: Array[Int], a: Int, b: Int) {
    val t = x(a)
    x(a) = x(b)
    x(b) = t
  }

  /**
   * Swaps x[a] with x[b].
   */
  private def swap(x: Array[Long], a: Int, b: Int) {
    val t = x(a)
    x(a) = x(b)
    x(b) = t
  }

  /**
   * Swaps x[a] with x[b].
   */
  private def swap[T](x: Array[T], a: Int, b: Int) {
    val t = x(a)
    x(a) = x(b)
    x(b) = t
  }

  /**
   * Swaps x[a] with x[b].
   */
  private def swap(x: Array[Short], a: Int, b: Int) {
    val t = x(a)
    x(a) = x(b)
    x(b) = t
  }

  /**
   * Swaps x[a .. (a+n-1)] with x[b .. (b+n-1)].
   */
  private def vecswap(x: Array[Byte],
      aP: Int,
      bP: Int,
      n: Int) {
    var a = aP
    var b = bP
    var i = 0
    while (i < n) {
      swap(x, a, b)
      i += 1
      a += 1
      b += 1
    }
  }

  /**
   * Swaps x[a .. (a+n-1)] with x[b .. (b+n-1)].
   */
  private def vecswap(x: Array[Char],
      aP: Int,
      bP: Int,
      n: Int) {
    var a = aP
    var b = bP
    var i = 0
    while (i < n) {
      swap(x, a, b)
      i += 1
      a += 1
      b += 1
    }
  }

  /**
   * Swaps x[a .. (a+n-1)] with x[b .. (b+n-1)].
   */
  private def vecswap(x: Array[Double],
      aP: Int,
      bP: Int,
      n: Int) {
    var a = aP
    var b = bP
    var i = 0
    while (i < n) {
      swap(x, a, b)
      i += 1
      a += 1
      b += 1
    }
  }

  /**
   * Swaps x[a .. (a+n-1)] with x[b .. (b+n-1)].
   */
  private def vecswap(x: Array[Float],
      aP: Int,
      bP: Int,
      n: Int) {
    var a = aP
    var b = bP
    var i = 0
    while (i < n) {
      swap(x, a, b)
      i += 1
      a += 1
      b += 1
    }
  }

  /**
   * Swaps x[a .. (a+n-1)] with x[b .. (b+n-1)].
   */
  private def vecswap(x: Array[Int],
      aP: Int,
      bP: Int,
      n: Int) {
    var a = aP
    var b = bP
    var i = 0
    while (i < n) {
      swap(x, a, b)
      i += 1
      a += 1
      b += 1
    }
  }

  /**
   * Swaps x[a .. (a+n-1)] with x[b .. (b+n-1)].
   */
  private def vecswap(x: Array[Long],
      aP: Int,
      bP: Int,
      n: Int) {
    var a = aP
    var b = bP
    var i = 0
    while (i < n) {
      swap(x, a, b)
      i += 1
      a += 1
      b += 1
    }
  }

  /**
   * Swaps x[a .. (a+n-1)] with x[b .. (b+n-1)].
   */
  private def vecswap[T](x: Array[T],
      aP: Int,
      bP: Int,
      n: Int) {
    var a = aP
    var b = bP
    var i = 0
    while (i < n) {
      swap(x, a, b)
      i += 1
      a += 1
      b += 1
    }
  }

  /**
   * Swaps x[a .. (a+n-1)] with x[b .. (b+n-1)].
   */
  private def vecswap(x: Array[Short],
      aP: Int,
      bP: Int,
      n: Int) {
    var a = aP
    var b = bP
    var i = 0
    while (i < n) {
      swap(x, a, b)
      i += 1
      a += 1
      b += 1
    }
  }

  /**
   * Returns the index of the median of the three indexed chars.
   */
  private def med3(x: Array[Byte],
      a: Int,
      b: Int,
      c: Int,
      comp: ByteComparator): Int = {
    val ab = comp.compare(x(a), x(b))
    val ac = comp.compare(x(a), x(c))
    val bc = comp.compare(x(b), x(c))
    (if (ab < 0) (if (bc < 0) b else if (ac < 0) c else a) else (if (bc > 0) b else if (ac > 0) c else a))
  }

  /**
   * Returns the index of the median of the three indexed chars.
   */
  private def med3(x: Array[Char],
      a: Int,
      b: Int,
      c: Int,
      comp: CharComparator): Int = {
    val ab = comp.compare(x(a), x(b))
    val ac = comp.compare(x(a), x(c))
    val bc = comp.compare(x(b), x(c))
    (if (ab < 0) (if (bc < 0) b else if (ac < 0) c else a) else (if (bc > 0) b else if (ac > 0) c else a))
  }

  /**
   * Returns the index of the median of the three indexed chars.
   */
  private def med3(x: Array[Double],
      a: Int,
      b: Int,
      c: Int,
      comp: PrimitiveComparator): Int = {
    val ab = comp.compare(x(a), x(b))
    val ac = comp.compare(x(a), x(c))
    val bc = comp.compare(x(b), x(c))
    (if (ab < 0) (if (bc < 0) b else if (ac < 0) c else a) else (if (bc > 0) b else if (ac > 0) c else a))
  }

  /**
   * Returns the index of the median of the three indexed chars.
   */
  private def med3(x: Array[Float],
      a: Int,
      b: Int,
      c: Int,
      comp: FloatComparator): Int = {
    val ab = comp.compare(x(a), x(b))
    val ac = comp.compare(x(a), x(c))
    val bc = comp.compare(x(b), x(c))
    (if (ab < 0) (if (bc < 0) b else if (ac < 0) c else a) else (if (bc > 0) b else if (ac > 0) c else a))
  }

  /**
   * Returns the index of the median of the three indexed chars.
   */
  private def med3(x: Array[Int],
      a: Int,
      b: Int,
      c: Int,
      comp: IntComparator): Int = {
    val ab = comp.compare(x(a), x(b))
    val ac = comp.compare(x(a), x(c))
    val bc = comp.compare(x(b), x(c))
    (if (ab < 0) (if (bc < 0) b else if (ac < 0) c else a) else (if (bc > 0) b else if (ac > 0) c else a))
  }

  /**
   * Returns the index of the median of the three indexed chars.
   */
  private def med3(x: Array[Long],
      a: Int,
      b: Int,
      c: Int,
      comp: LongComparator): Int = {
    val ab = comp.compare(x(a), x(b))
    val ac = comp.compare(x(a), x(c))
    val bc = comp.compare(x(b), x(c))
    (if (ab < 0) (if (bc < 0) b else if (ac < 0) c else a) else (if (bc > 0) b else if (ac > 0) c else a))
  }

  /**
   * Returns the index of the median of the three indexed chars.
   */
  private def med3[T <: Comparable[T]](x: Array[T],
      a: Int,
      b: Int,
      c: Int): Int = {
    val ab = x(a).compareTo(x(b))
    val ac = x(a).compareTo(x(c))
    val bc = x(b).compareTo(x(c))
    (if (ab < 0) (if (bc < 0) b else if (ac < 0) c else a) else (if (bc > 0) b else if (ac > 0) c else a))
  }

  /**
   * Returns the index of the median of the three indexed chars.
   */
  private def med3[T](x: Array[T],
      a: Int,
      b: Int,
      c: Int,
      comp: Comparator[T]): Int = {
    val ab = comp.compare(x(a), x(b))
    val ac = comp.compare(x(a), x(c))
    val bc = comp.compare(x(b), x(c))
    (if (ab < 0) (if (bc < 0) b else if (ac < 0) c else a) else (if (bc > 0) b else if (ac > 0) c else a))
  }

  /**
   * Returns the index of the median of the three indexed chars.
   */
  private def med3(x: Array[Short],
      a: Int,
      b: Int,
      c: Int,
      comp: ShortComparator): Int = {
    val ab = comp.compare(x(a), x(b))
    val ac = comp.compare(x(a), x(c))
    val bc = comp.compare(x(b), x(c))
    (if (ab < 0) (if (bc < 0) b else if (ac < 0) c else a) else (if (bc > 0) b else if (ac > 0) c else a))
  }
}
