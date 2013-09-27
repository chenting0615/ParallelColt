package cern.colt.matrix.tdouble.algo.decomposition

import cern.colt.matrix.tdouble.DoubleFactory1D
import cern.colt.matrix.tdouble.DoubleFactory2D
import cern.colt.matrix.tdouble.StrideMatrix1D
import cern.colt.matrix.tdouble.StrideMatrix2D
import cern.colt.matrix.tdouble.algo.DenseDoubleAlgebra
import cern.colt.matrix.tdouble.algo.DoubleProperty
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Eigenvalues and eigenvectors of a real matrix <tt>A</tt>.
 * <P>
 * If <tt>A</tt> is symmetric, then <tt>A = V*D*V'</tt> where the eigenvalue matrix
 * <tt>D</tt> is diagonal and the eigenvector matrix <tt>V</tt> is orthogonal.
 * I.e. <tt>A = V.mult(D.mult(transpose(V)))</tt> and
 * <tt>V.mult(transpose(V))</tt> equals the identity matrix.
 *
 * <P>
 * If <tt>A</tt> is not symmetric, then the eigenvalue matrix <tt>D</tt> is
 * block diagonal with the real eigenvalues in 1-by-1 blocks and any complex
 * eigenvalues, <tt>lambda + i*mu</tt>, in 2-by-2 blocks,
 * <tt>[lambda, mu; -mu, lambda]</tt>. The columns of <tt>V</tt> represent the
 * eigenvectors in the sense that <tt>A*V = V*D</tt>, i.e.
 * <tt>A.mult(V) equals V.mult(D)</tt>. The matrix <tt>V</tt> may be badly
 * conditioned, or even singular, so the validity of the equation
 * <tt>A = V*D*inverse(V)</tt> depends upon <tt>Algebra.cond(V)</tt>.
 */
@SerialVersionUID(1020)
class DenseDoubleEigenvalueDecomposition(A: StrideMatrix2D) extends java.io.Serializable {

  /**
   * Row and column dimension (square matrix).
   *
   * @serial matrix dimension.
   */
  private var n: Int = A.columns()

  /**
   * Symmetry flag.
   *
   * @serial internal symmetry flag.
   */
  private var issymmetric: Boolean = DoubleProperty.DEFAULT.isSymmetric(A)

  /**
   * Arrays for internal storage of eigenvalues.
   *
   * @serial internal storage of eigenvalues.
   */
  private var d: Array[Double] = new Array[Double](n)

  private var e: Array[Double] = new Array[Double](n)

  /**
   * Array for internal storage of eigenvectors.
   *
   * @serial internal storage of eigenvectors.
   */
  private var V: Array[Array[Double]] = new Array[Array[Double]](n, n)

  /**
   * Array for internal storage of nonsymmetric Hessenberg form.
   *
   * @serial internal storage of nonsymmetric Hessenberg form.
   */
  private var H: Array[Array[Double]] = _

  /**
   * Working storage for nonsymmetric algorithm.
   *
   * @serial working storage for nonsymmetric algorithm.
   */
  private var ort: Array[Double] = _

  @transient private var cdivr: Double = _

  @transient private var cdivi: Double = _

  DoubleProperty.DEFAULT.checkSquare(A)

  if (issymmetric) {
    for (i <- 0 until n; j <- 0 until n) {
      V(i)(j) = A.getQuick(i, j)
    }
    tred2()
    tql2()
  } else {
    H = Array.ofDim[Double](n, n)
    ort = Array.ofDim[Double](n)
    for (j <- 0 until n; i <- 0 until n) {
      H(i)(j) = A.getQuick(i, j)
    }
    orthes()
    hqr2()
  }

  private def cdiv(xr: Double,
      xi: Double,
      yr: Double,
      yi: Double) {
    var r: Double = 0.0
    var d: Double = 0.0
    if (Math.abs(yr) > Math.abs(yi)) {
      r = yi / yr
      d = yr + r * yi
      cdivr = (xr + r * xi) / d
      cdivi = (xi - r * xr) / d
    } else {
      r = yr / yi
      d = yi + r * yr
      cdivr = (r * xr + xi) / d
      cdivi = (r * xi - xr) / d
    }
  }

  /**
   * Returns the block diagonal eigenvalue matrix, <tt>D</tt>.
   *
   * @return <tt>D</tt>
   */
  def getD(): StrideMatrix2D = {
    val D = Array.ofDim[Double](n, n)
    for (i <- 0 until n) {
      for (j <- 0 until n) {
        D(i)(j) = 0.0
      }
      D(i)(i) = d(i)
      if (e(i) > 0) {
        D(i)(i + 1) = e(i)
      } else if (e(i) < 0) {
        D(i)(i - 1) = e(i)
      }
    }
    DoubleFactory2D.dense.make(D)
  }

  /**
   * Returns the imaginary parts of the eigenvalues.
   *
   * @return imag(diag(D))
   */
  def getImagEigenvalues(): StrideMatrix1D = DoubleFactory1D.dense.make(e)

  /**
   * Returns the real parts of the eigenvalues.
   *
   * @return real(diag(D))
   */
  def getRealEigenvalues(): StrideMatrix1D = DoubleFactory1D.dense.make(d)

  /**
   * Returns the eigenvector matrix, <tt>V</tt>
   *
   * @return <tt>V</tt>
   */
  def getV(): StrideMatrix2D = DoubleFactory2D.dense.make(V)

  /**
   * Nonsymmetric reduction from Hessenberg to real Schur form.
   */
  private def hqr2() {
    val nn = this.n
    var n = nn - 1
    val low = 0
    val high = nn - 1
    val eps = Math.pow(2.0, -52.0)
    var exshift = 0.0
    var p = 0
    var q = 0
    var r = 0
    var s = 0
    var z = 0
    var t: Double = 0.0
    var w: Double = 0.0
    var x: Double = 0.0
    var y: Double = 0.0
    var norm = 0.0
    for (i <- 0 until nn) {
      if (i < low | i > high) {
        d(i) = H(i)(i)
        e(i) = 0.0
      }
      for (j <- Math.max(i - 1, 0) until nn) {
        norm = norm + Math.abs(H(i)(j))
      }
    }
    var iter = 0
    while (n >= low) {
      var l = n
      while (l > low) {
        s = Math.abs(H(l - 1)(l - 1)) + Math.abs(H(l)(l))
        if (s == 0.0) {
          s = norm
        }
        if (Math.abs(H(l)(l - 1)) < eps * s) {
          //break
        }
        l -= 1
      }
      if (l == n) {
        H(n)(n) = H(n)(n) + exshift
        d(n) = H(n)(n)
        e(n) = 0.0
        n -= 1
        iter = 0
      } else if (l == n - 1) {
        w = H(n)(n - 1) * H(n - 1)(n)
        p = (H(n - 1)(n - 1) - H(n)(n)) / 2.0
        q = p * p + w
        z = Math.sqrt(Math.abs(q))
        H(n)(n) = H(n)(n) + exshift
        H(n - 1)(n - 1) = H(n - 1)(n - 1) + exshift
        x = H(n)(n)
        if (q >= 0) {
          z = if (p >= 0) p + z else p - z
          d(n - 1) = x + z
          d(n) = d(n - 1)
          if (z != 0.0) {
            d(n) = x - w / z
          }
          e(n - 1) = 0.0
          e(n) = 0.0
          x = H(n)(n - 1)
          s = Math.abs(x) + Math.abs(z)
          p = x / s
          q = z / s
          r = Math.sqrt(p * p + q * q)
          p = p / r
          q = q / r
          for (j <- n - 1 until nn) {
            z = H(n - 1)(j)
            H(n - 1)(j) = q * z + p * H(n)(j)
            H(n)(j) = q * H(n)(j) - p * z
          }
          var i = 0
          while (i <= n) {
            z = H(i)(n - 1)
            H(i)(n - 1) = q * z + p * H(i)(n)
            H(i)(n) = q * H(i)(n) - p * z
            i += 1
          }
          var i = low
          while (i <= high) {
            z = V(i)(n - 1)
            V(i)(n - 1) = q * z + p * V(i)(n)
            V(i)(n) = q * V(i)(n) - p * z
            i += 1
          }
        } else {
          d(n - 1) = x + p
          d(n) = x + p
          e(n - 1) = z
          e(n) = -z
        }
        n = n - 2
        iter = 0
      } else {
        x = H(n)(n)
        y = 0.0
        w = 0.0
        if (l < n) {
          y = H(n - 1)(n - 1)
          w = H(n)(n - 1) * H(n - 1)(n)
        }
        if (iter == 10) {
          exshift += x
          var i = low
          while (i <= n) {
            H(i)(i) -= x
            i += 1
          }
          s = Math.abs(H(n)(n - 1)) + Math.abs(H(n - 1)(n - 2))
          x = y = 0.75 * s
          w = -0.4375 * s * s
        }
        if (iter == 30) {
          s = (y - x) / 2.0
          s = s * s + w
          if (s > 0) {
            s = Math.sqrt(s)
            if (y < x) {
              s = -s
            }
            s = x - w / ((y - x) / 2.0 + s)
            var i = low
            while (i <= n) {
              H(i)(i) -= s
              i += 1
            }
            exshift += s
            x = y = w = 0.964
          }
        }
        iter = iter + 1
        var m = n - 2
        while (m >= l) {
          z = H(m)(m)
          r = x - z
          s = y - z
          p = (r * s - w) / H(m + 1)(m) + H(m)(m + 1)
          q = H(m + 1)(m + 1) - z - r - s
          r = H(m + 2)(m + 1)
          s = Math.abs(p) + Math.abs(q) + Math.abs(r)
          p = p / s
          q = q / s
          r = r / s
          if (m == l) {
            //break
          }
          if (Math.abs(H(m)(m - 1)) * (Math.abs(q) + Math.abs(r)) <
            eps *
            (Math.abs(p) *
            (Math.abs(H(m - 1)(m - 1)) + Math.abs(z) + Math.abs(H(m + 1)(m + 1))))) {
            //break
          }
          m -= 1
        }
        var i = m + 2
        while (i <= n) {
          H(i)(i - 2) = 0.0
          if (i > m + 2) {
            H(i)(i - 3) = 0.0
          }
          i += 1
        }
        var k = m
        while (k <= n - 1) {
          val notlast = (k != n - 1)
          if (k != m) {
            p = H(k)(k - 1)
            q = H(k + 1)(k - 1)
            r = (if (notlast) H(k + 2)(k - 1) else 0.0)
            x = Math.abs(p) + Math.abs(q) + Math.abs(r)
            if (x != 0.0) {
              p = p / x
              q = q / x
              r = r / x
            }
          }
          if (x == 0.0) {
            //break
          }
          s = Math.sqrt(p * p + q * q + r * r)
          if (p < 0) {
            s = -s
          }
          if (s != 0) {
            if (k != m) {
              H(k)(k - 1) = -s * x
            } else if (l != m) {
              H(k)(k - 1) = -H(k)(k - 1)
            }
            p = p + s
            x = p / s
            y = q / s
            z = r / s
            q = q / p
            r = r / p
            for (j <- k until nn) {
              p = H(k)(j) + q * H(k + 1)(j)
              if (notlast) {
                p = p + r * H(k + 2)(j)
                H(k + 2)(j) = H(k + 2)(j) - p * z
              }
              H(k)(j) = H(k)(j) - p * x
              H(k + 1)(j) = H(k + 1)(j) - p * y
            }
            var i = 0
            while (i <= Math.min(n, k + 3)) {
              p = x * H(i)(k) + y * H(i)(k + 1)
              if (notlast) {
                p = p + z * H(i)(k + 2)
                H(i)(k + 2) = H(i)(k + 2) - p * r
              }
              H(i)(k) = H(i)(k) - p
              H(i)(k + 1) = H(i)(k + 1) - p * q
              i += 1
            }
            var i = low
            while (i <= high) {
              p = x * V(i)(k) + y * V(i)(k + 1)
              if (notlast) {
                p = p + z * V(i)(k + 2)
                V(i)(k + 2) = V(i)(k + 2) - p * r
              }
              V(i)(k) = V(i)(k) - p
              V(i)(k + 1) = V(i)(k + 1) - p * q
              i += 1
            }
          }
          k += 1
        }
      }
    }
    if (norm == 0.0) {
      return
    }
    n = nn - 1
    while (n >= 0) {
      p = d(n)
      q = e(n)
      if (q == 0) {
        var l = n
        H(n)(n) = 1.0
        var i = n - 1
        while (i >= 0) {
          w = H(i)(i) - p
          r = 0.0
          var j = l
          while (j <= n) {
            r = r + H(i)(j) * H(j)(n)
            j += 1
          }
          if (e(i) < 0.0) {
            z = w
            s = r
          } else {
            l = i
            if (e(i) == 0.0) {
              H(i)(n) = if (w != 0.0) -r / w else -r / (eps * norm)
            } else {
              x = H(i)(i + 1)
              y = H(i + 1)(i)
              q = (d(i) - p) * (d(i) - p) + e(i) * e(i)
              t = (x * s - z * r) / q
              H(i)(n) = t
              H(i + 1)(n) = if (Math.abs(x) > Math.abs(z)) (-r - w * t) / x else (-s - y * t) / z
            }
            t = Math.abs(H(i)(n))
            if ((eps * t) * t > 1) {
              var j = i
              while (j <= n) {
                H(j)(n) = H(j)(n) / t
                j += 1
              }
            }
          }
          i -= 1
        }
      } else if (q < 0) {
        var l = n - 1
        if (Math.abs(H(n)(n - 1)) > Math.abs(H(n - 1)(n))) {
          H(n - 1)(n - 1) = q / H(n)(n - 1)
          H(n - 1)(n) = -(H(n)(n) - p) / H(n)(n - 1)
        } else {
          cdiv(0.0, -H(n - 1)(n), H(n - 1)(n - 1) - p, q)
          H(n - 1)(n - 1) = cdivr
          H(n - 1)(n) = cdivi
        }
        H(n)(n - 1) = 0.0
        H(n)(n) = 1.0
        var i = n - 2
        while (i >= 0) {
          var ra: Double = 0.0
          var sa: Double = 0.0
          var vr: Double = 0.0
          var vi: Double = 0.0
          ra = 0.0
          sa = 0.0
          var j = l
          while (j <= n) {
            ra = ra + H(i)(j) * H(j)(n - 1)
            sa = sa + H(i)(j) * H(j)(n)
            j += 1
          }
          w = H(i)(i) - p
          if (e(i) < 0.0) {
            z = w
            r = ra
            s = sa
          } else {
            l = i
            if (e(i) == 0) {
              cdiv(-ra, -sa, w, q)
              H(i)(n - 1) = cdivr
              H(i)(n) = cdivi
            } else {
              x = H(i)(i + 1)
              y = H(i + 1)(i)
              vr = (d(i) - p) * (d(i) - p) + e(i) * e(i) - q * q
              vi = (d(i) - p) * 2.0 * q
              if (vr == 0.0 & vi == 0.0) {
                vr = eps * norm *
                  (Math.abs(w) + Math.abs(q) + Math.abs(x) + Math.abs(y) +
                  Math.abs(z))
              }
              cdiv(x * r - z * ra + q * sa, x * s - z * sa - q * ra, vr, vi)
              H(i)(n - 1) = cdivr
              H(i)(n) = cdivi
              if (Math.abs(x) > (Math.abs(z) + Math.abs(q))) {
                H(i + 1)(n - 1) = (-ra - w * H(i)(n - 1) + q * H(i)(n)) / x
                H(i + 1)(n) = (-sa - w * H(i)(n) - q * H(i)(n - 1)) / x
              } else {
                cdiv(-r - y * H(i)(n - 1), -s - y * H(i)(n), z, q)
                H(i + 1)(n - 1) = cdivr
                H(i + 1)(n) = cdivi
              }
            }
            t = Math.max(Math.abs(H(i)(n - 1)), Math.abs(H(i)(n)))
            if ((eps * t) * t > 1) {
              var j = i
              while (j <= n) {
                H(j)(n - 1) = H(j)(n - 1) / t
                H(j)(n) = H(j)(n) / t
                j += 1
              }
            }
          }
          i -= 1
        }
      }
      n -= 1
    }
    for (i <- 0 until nn if i < low | i > high; j <- i until nn) {
      V(i)(j) = H(i)(j)
    }
    var j = nn - 1
    while (j >= low) {
      var i = low
      while (i <= high) {
        z = 0.0
        var k = low
        while (k <= Math.min(j, high)) {
          z = z + V(i)(k) * H(k)(j)
          k += 1
        }
        V(i)(j) = z
        i += 1
      }
      j -= 1
    }
  }

  /**
   * Nonsymmetric reduction to Hessenberg form.
   */
  private def orthes() {
    val low = 0
    val high = n - 1
    var m = low + 1
    while (m <= high - 1) {
      var scale = 0.0
      var i = m
      while (i <= high) {
        scale = scale + Math.abs(H(i)(m - 1))
        i += 1
      }
      if (scale != 0.0) {
        var h = 0.0
        var i = high
        while (i >= m) {
          ort(i) = H(i)(m - 1) / scale
          h += ort(i) * ort(i)
          i -= 1
        }
        var g = Math.sqrt(h)
        if (ort(m) > 0) {
          g = -g
        }
        h = h - ort(m) * g
        ort(m) = ort(m) - g
        for (j <- m until n) {
          var f = 0.0
          var i = high
          while (i >= m) {
            f += ort(i) * H(i)(j)
            i -= 1
          }
          f = f / h
          var i = m
          while (i <= high) {
            H(i)(j) -= f * ort(i)
            i += 1
          }
        }
        var i = 0
        while (i <= high) {
          var f = 0.0
          var j = high
          while (j >= m) {
            f += ort(j) * H(i)(j)
            j -= 1
          }
          f = f / h
          var j = m
          while (j <= high) {
            H(i)(j) -= f * ort(j)
            j += 1
          }
          i += 1
        }
        ort(m) = scale * ort(m)
        H(m)(m - 1) = scale * g
      }
      m += 1
    }
    for (i <- 0 until n; j <- 0 until n) {
      V(i)(j) = (if (i == j) 1.0 else 0.0)
    }
    var m = high - 1
    while (m >= low + 1) {
      if (H(m)(m - 1) != 0.0) {
        var i = m + 1
        while (i <= high) {
          ort(i) = H(i)(m - 1)
          i += 1
        }
        var j = m
        while (j <= high) {
          var g = 0.0
          var i = m
          while (i <= high) {
            g += ort(i) * V(i)(j)
            i += 1
          }
          g = (g / ort(m)) / H(m)(m - 1)
          var i = m
          while (i <= high) {
            V(i)(j) += g * ort(i)
            i += 1
          }
          j += 1
        }
      }
      m -= 1
    }
  }

  /**
   * Returns a String with (propertyName, propertyValue) pairs. Useful for
   * debugging or to quickly get the rough picture. For example,
   *
   * <pre>
   * 	 rank          : 3
   * 	 trace         : 0
   *
   * </pre>
   */
  override def toString(): String = {
    val buf = new StringBuffer()
    val unknown = "Illegal operation or error: "
    buf.append("---------------------------------------------------------------------\n")
    buf.append("EigenvalueDecomposition(A) --> D, V, realEigenvalues, imagEigenvalues\n")
    buf.append("---------------------------------------------------------------------\n")
    buf.append("realEigenvalues = ")
    try {
      buf.append(String.valueOf(this.getRealEigenvalues))
    } catch {
      case exc: IllegalArgumentException => buf.append(unknown + exc.getMessage)
    }
    buf.append("\nimagEigenvalues = ")
    try {
      buf.append(String.valueOf(this.getImagEigenvalues))
    } catch {
      case exc: IllegalArgumentException => buf.append(unknown + exc.getMessage)
    }
    buf.append("\n\nD = ")
    try {
      buf.append(String.valueOf(this.getD))
    } catch {
      case exc: IllegalArgumentException => buf.append(unknown + exc.getMessage)
    }
    buf.append("\n\nV = ")
    try {
      buf.append(String.valueOf(this.getV))
    } catch {
      case exc: IllegalArgumentException => buf.append(unknown + exc.getMessage)
    }
    buf.toString
  }

  /**
   * Symmetric tridiagonal QL algorithm.
   */
  private def tql2() {
    for (i <- 1 until n) {
      e(i - 1) = e(i)
    }
    e(n - 1) = 0.0
    var f = 0.0
    var tst1 = 0.0
    val eps = Math.pow(2.0, -52.0)
    for (l <- 0 until n) {
      tst1 = Math.max(tst1, Math.abs(d(l)) + Math.abs(e(l)))
      var m = l
      while (m < n) {
        if (Math.abs(e(m)) <= eps * tst1) {
          //break
        }
        m += 1
      }
      if (m > l) {
        var iter = 0
        do {
          iter = iter + 1
          var g = d(l)
          var p = (d(l + 1) - g) / (2.0 * e(l))
          var r = DenseDoubleAlgebra.hypot(p, 1.0)
          if (p < 0) {
            r = -r
          }
          d(l) = e(l) / (p + r)
          d(l + 1) = e(l) * (p + r)
          val dl1 = d(l + 1)
          var h = g - d(l)
          for (i <- l + 2 until n) {
            d(i) -= h
          }
          f = f + h
          p = d(m)
          var c = 1.0
          var c2 = c
          var c3 = c
          val el1 = e(l + 1)
          var s = 0.0
          var s2 = 0.0
          var i = m - 1
          while (i >= l) {
            c3 = c2
            c2 = c
            s2 = s
            g = c * e(i)
            h = c * p
            r = DenseDoubleAlgebra.hypot(p, e(i))
            e(i + 1) = s * r
            s = e(i) / r
            c = p / r
            p = c * d(i) - s * g
            d(i + 1) = h + s * (c * g + s * d(i))
            for (k <- 0 until n) {
              h = V(k)(i + 1)
              V(k)(i + 1) = s * V(k)(i) + c * h
              V(k)(i) = c * V(k)(i) - s * h
            }
            i -= 1
          }
          p = -s * s2 * c3 * el1 * e(l) / dl1
          e(l) = s * p
          d(l) = c * p
        } while (Math.abs(e(l)) > eps * tst1);
      }
      d(l) = d(l) + f
      e(l) = 0.0
    }
    for (i <- 0 until n - 1) {
      var k = i
      var p = d(i)
      for (j <- i + 1 until n if d(j) < p) {
        k = j
        p = d(j)
      }
      if (k != i) {
        d(k) = d(i)
        d(i) = p
        for (j <- 0 until n) {
          p = V(j)(i)
          V(j)(i) = V(j)(k)
          V(j)(k) = p
        }
      }
    }
  }

  /**
   * Symmetric Householder reduction to tridiagonal form.
   */
  private def tred2() {
    for (j <- 0 until n) {
      d(j) = V(n - 1)(j)
    }
    var i = n - 1
    while (i > 0) {
      var scale = 0.0
      var h = 0.0
      for (k <- 0 until i) {
        scale = scale + Math.abs(d(k))
      }
      if (scale == 0.0) {
        e(i) = d(i - 1)
        for (j <- 0 until i) {
          d(j) = V(i - 1)(j)
          V(i)(j) = 0.0
          V(j)(i) = 0.0
        }
      } else {
        for (k <- 0 until i) {
          d(k) /= scale
          h += d(k) * d(k)
        }
        var f = d(i - 1)
        var g = Math.sqrt(h)
        if (f > 0) {
          g = -g
        }
        e(i) = scale * g
        h = h - f * g
        d(i - 1) = f - g
        for (j <- 0 until i) {
          e(j) = 0.0
        }
        for (j <- 0 until i) {
          f = d(j)
          V(j)(i) = f
          g = e(j) + V(j)(j) * f
          var k = j + 1
          while (k <= i - 1) {
            g += V(k)(j) * d(k)
            e(k) += V(k)(j) * f
            k += 1
          }
          e(j) = g
        }
        f = 0.0
        for (j <- 0 until i) {
          e(j) /= h
          f += e(j) * d(j)
        }
        val hh = f / (h + h)
        for (j <- 0 until i) {
          e(j) -= hh * d(j)
        }
        for (j <- 0 until i) {
          f = d(j)
          g = e(j)
          var k = j
          while (k <= i - 1) {
            V(k)(j) -= (f * e(k) + g * d(k))
            k += 1
          }
          d(j) = V(i - 1)(j)
          V(i)(j) = 0.0
        }
      }
      d(i) = h
      i -= 1
    }
    for (i <- 0 until n - 1) {
      V(n - 1)(i) = V(i)(i)
      V(i)(i) = 1.0
      val h = d(i + 1)
      if (h != 0.0) {
        var k = 0
        while (k <= i) {
          d(k) = V(k)(i + 1) / h
          k += 1
        }
        var j = 0
        while (j <= i) {
          var g = 0.0
          var k = 0
          while (k <= i) {
            g += V(k)(i + 1) * V(k)(j)
            k += 1
          }
          var k = 0
          while (k <= i) {
            V(k)(j) -= g * d(k)
            k += 1
          }
          j += 1
        }
      }
      var k = 0
      while (k <= i) {
        V(k)(i + 1) = 0.0
        k += 1
      }
    }
    for (j <- 0 until n) {
      d(j) = V(n - 1)(j)
      V(n - 1)(j) = 0.0
    }
    V(n - 1)(n - 1) = 1.0
    e(0) = 0.0
  }
}
