package edu.emory.mathcs.utils

import java.io.BufferedWriter
import java.io.FileWriter
import java.io.IOException
import java.util.Date
import java.util.Random
//remove if not needed
import scala.collection.JavaConversions._

object IOUtils {

  /**
   * Fills 1D matrix with random numbers.
   *
   * @param N
   *            size
   * @param m
   *            1D matrix
   */
  def fillMatrix_1D(N: Int, m: Array[Double]) {
    val r = new Random(2)
    for (i <- 0 until N) {
      m(i) = r.nextDouble()
    }
  }

  /**
   * Fills 1D matrix with random numbers.
   *
   * @param N
   *            size
   * @param m
   *            1D matrix
   */
  def fillMatrix_1D(N: Int, m: Array[Float]) {
    val r = new Random(2)
    for (i <- 0 until N) {
      m(i) = r.nextFloat()
    }
  }

  /**
   * Fills 2D matrix with random numbers.
   *
   * @param n1
   *            rows
   * @param n2
   *            columns
   * @param m
   *            2D matrix
   */
  def fillMatrix_2D(n1: Int, n2: Int, m: Array[Double]) {
    val r = new Random(2)
    for (i <- 0 until n1; j <- 0 until n2) {
      m(i * n2 + j) = r.nextDouble()
    }
  }

  /**
   * Fills 2D matrix with random numbers.
   *
   * @param n1
   *            rows
   * @param n2
   *            columns
   * @param m
   *            2D matrix
   */
  def fillMatrix_2D(n1: Int, n2: Int, m: Array[Float]) {
    val r = new Random(2)
    for (i <- 0 until n1; j <- 0 until n2) {
      m(i * n2 + j) = r.nextFloat()
    }
  }

  /**
   * Fills 2D matrix with random numbers.
   *
   * @param n1
   *            rows
   * @param n2
   *            columns
   * @param m
   *            2D matrix
   */
  def fillMatrix_2D(n1: Int, n2: Int, m: Array[Array[Double]]) {
    val r = new Random(2)
    for (i <- 0 until n1; j <- 0 until n2) {
      m(i)(j) = r.nextDouble()
    }
  }

  /**
   * Fills 2D matrix with random numbers.
   *
   * @param n1
   *            rows
   * @param n2
   *            columns
   * @param m
   *            2D matrix
   */
  def fillMatrix_2D(n1: Int, n2: Int, m: Array[Array[Float]]) {
    val r = new Random(2)
    for (i <- 0 until n1; j <- 0 until n2) {
      m(i)(j) = r.nextFloat()
    }
  }

  /**
   * Fills 3D matrix with random numbers.
   *
   * @param n1
   *            slices
   * @param n2
   *            rows
   * @param n3
   *            columns
   * @param m
   *            3D matrix
   */
  def fillMatrix_3D(n1: Int, 
      n2: Int, 
      n3: Int, 
      m: Array[Double]) {
    val r = new Random(2)
    val sliceStride = n2 * n3
    val rowStride = n3
    for (i <- 0 until n1; j <- 0 until n2; k <- 0 until n3) {
      m(i * sliceStride + j * rowStride + k) = r.nextDouble()
    }
  }

  /**
   * Fills 3D matrix with random numbers.
   *
   * @param n1
   *            slices
   * @param n2
   *            rows
   * @param n3
   *            columns
   * @param m
   *            3D matrix
   */
  def fillMatrix_3D(n1: Int, 
      n2: Int, 
      n3: Int, 
      m: Array[Float]) {
    val r = new Random(2)
    val sliceStride = n2 * n3
    val rowStride = n3
    for (i <- 0 until n1; j <- 0 until n2; k <- 0 until n3) {
      m(i * sliceStride + j * rowStride + k) = r.nextFloat()
    }
  }

  /**
   * Fills 3D matrix with random numbers.
   *
   * @param n1
   *            slices
   * @param n2
   *            rows
   * @param n3
   *            columns
   * @param m
   *            3D matrix
   */
  def fillMatrix_3D(n1: Int, 
      n2: Int, 
      n3: Int, 
      m: Array[Array[Array[Double]]]) {
    val r = new Random(2)
    for (i <- 0 until n1; j <- 0 until n2; k <- 0 until n3) {
      m(i)(j)(k) = r.nextDouble()
    }
  }

  /**
   * Fills 3D matrix with random numbers.
   *
   * @param n1
   *            slices
   * @param n2
   *            rows
   * @param n3
   *            columns
   * @param m
   *            3D matrix
   */
  def fillMatrix_3D(n1: Int, 
      n2: Int, 
      n3: Int, 
      m: Array[Array[Array[Float]]]) {
    val r = new Random(2)
    for (i <- 0 until n1; j <- 0 until n2; k <- 0 until n3) {
      m(i)(j)(k) = r.nextFloat()
    }
  }

  /**
   * Displays elements of <code>x</code>, assuming that it is 1D complex
   * array. Complex data is represented by 2 double values in sequence: the
   * real and imaginary parts.
   *
   * @param x
   * @param title
   */
  def showComplex_1D(format: String, x: Array[Double], title: String) {
    println(title)
    println("-------------------")
    var i = 0
    while (i < x.length) {
      if (x(i + 1) == 0) {
        println(String.format(format, x(i)))
        //continue
      }
      if (x(i) == 0) {
        println(String.format(format, x(i + 1)) + "i")
        //continue
      }
      if (x(i + 1) < 0) {
        println(String.format(format, x(i)) + " - " + (String.format(format, -x(i + 1))) + 
          "i")
        //continue
      }
      println(String.format(format, x(i)) + " + " + (String.format(format, x(i + 1))) + 
        "i")
      i = i + 2
    }
    println()
  }

  /**
   * Displays elements of <code>x</code>, assuming that it is 2D complex
   * array. Complex data is represented by 2 double values in sequence: the
   * real and imaginary parts.
   *
   * @param rows
   * @param columns
   * @param x
   * @param title
   */
  def showComplex_2D(format: String, 
      rows: Int, 
      columns: Int, 
      x: Array[Double], 
      title: String) {
    val s = new StringBuffer(String.format(title + ": complex array 2D: %d rows, %d columns\n\n", rows, 
      columns))
    for (r <- 0 until rows) {
      var c = 0
      while (c < 2 * columns) {
        if (x(r * 2 * columns + c + 1) == 0) {
          s.append(String.format(format + "\t", x(r * 2 * columns + c)))
          //continue
        }
        if (x(r * 2 * columns + c) == 0) {
          s.append(String.format(format + "i\t", x(r * 2 * columns + c + 1)))
          //continue
        }
        if (x(r * 2 * columns + c + 1) < 0) {
          s.append(String.format(format + " - " + format + "i\t", x(r * 2 * columns + c), -x(r * 2 * columns + c + 1)))
          //continue
        }
        s.append(String.format(format + " + " + format + "i\t", x(r * 2 * columns + c), x(r * 2 * columns + c + 1)))
        c = c + 2
      }
      s.append("\n")
    }
    println(s.toString)
  }

  /**
   * Displays elements of <code>x</code>, assuming that it is 3D complex
   * array. Complex data is represented by 2 double values in sequence: the
   * real and imaginary parts.
   *
   * @param n1
   * @param n2
   * @param n3
   * @param x
   * @param title
   */
  def showComplex_3D(format: String, 
      n1: Int, 
      n2: Int, 
      n3: Int, 
      x: Array[Double], 
      title: String) {
    val sliceStride = n2 * 2 * n3
    val rowStride = 2 * n3
    println(title)
    println("-------------------")
    var k = 0
    while (k < 2 * n3) {
      println("(:,:," + k / 2 + ")=\n")
      for (i <- 0 until n1) {
        for (j <- 0 until n2) {
          if (x(i * sliceStride + j * rowStride + k + 1) == 0) {
            System.out.print(String.format(format, x(i * sliceStride + j * rowStride + k)) + 
              "\t")
            //continue
          }
          if (x(i * sliceStride + j * rowStride + k) == 0) {
            System.out.print(String.format(format, x(i * sliceStride + j * rowStride + k + 1)) + 
              "i\t")
            //continue
          }
          if (x(i * sliceStride + j * rowStride + k + 1) < 0) {
            System.out.print(String.format(format, x(i * sliceStride + j * rowStride + k)) + 
              " - " + 
              String.format(format, -x(i * sliceStride + j * rowStride + k + 1)) + 
              "i\t")
            //continue
          }
          System.out.print(String.format(format, x(i * sliceStride + j * rowStride + k)) + 
            " + " + 
            String.format(format, x(i * sliceStride + j * rowStride + k + 1)) + 
            "i\t")
        }
        println("")
      }
      k = k + 2
    }
    println("")
  }

  /**
   * Displays elements of <code>x</code>, assuming that it is 1D real array.
   *
   * @param x
   * @param title
   */
  def showReal_1D(format: String, x: Array[Double], title: String) {
    println(title)
    println("-------------------")
    for (j <- 0 until x.length) {
      println(String.format(format, x(j)))
    }
    println()
  }

  /**
   * Displays elements of <code>x</code>, assuming that it is 2D real array.
   *
   * @param n1
   * @param n2
   * @param x
   * @param title
   */
  def showReal_2D(format: String, 
      n1: Int, 
      n2: Int, 
      x: Array[Double], 
      title: String) {
    println(title)
    println("-------------------")
    for (i <- 0 until n1) {
      for (j <- 0 until n2) {
        if (Math.abs(x(i * n2 + j)) < 5e-5) {
          System.out.print("0\t")
        } else {
          System.out.print(String.format(format, x(i * n2 + j)) + "\t")
        }
      }
      println()
    }
    println()
  }

  /**
   * Displays elements of <code>x</code>, assuming that it is 3D real array.
   *
   * @param n1
   * @param n2
   * @param n3
   * @param x
   * @param title
   */
  def showReal_3D(format: String, 
      n1: Int, 
      n2: Int, 
      n3: Int, 
      x: Array[Double], 
      title: String) {
    val sliceStride = n2 * n3
    val rowStride = n3
    println(title)
    println("-------------------")
    for (k <- 0 until n3) {
      println()
      println("(:,:," + k + ")=\n")
      for (i <- 0 until n1) {
        for (j <- 0 until n2) {
          if (Math.abs(x(i * sliceStride + j * rowStride + k)) <= 5e-5) {
            System.out.print("0\t")
          } else {
            System.out.print(String.format(format, x(i * sliceStride + j * rowStride + k)) + 
              "\t")
          }
        }
        println()
      }
    }
    println()
  }

  /**
   * Saves elements of <code>x</code> in a file <code>filename</code>,
   * assuming that it is 1D complex array. Complex data is represented by 2
   * double values in sequence: the real and imaginary parts.
   *
   * @param x
   * @param filename
   */
  def writeToFileComplex_1D(format: String, x: Array[Double], filename: String) {
    try {
      val out = new BufferedWriter(new FileWriter(filename))
      var i = 0
      while (i < x.length) {
        if (x(i + 1) == 0) {
          out.write(String.format(format, x(i)))
          out.newLine()
          //continue
        }
        if (x(i) == 0) {
          out.write(String.format(format, x(i + 1)) + "i")
          out.newLine()
          //continue
        }
        if (x(i + 1) < 0) {
          out.write(String.format(format, x(i)) + " - " + String.format(format, -x(i + 1)) + 
            "i")
          out.newLine()
          //continue
        }
        out.write(String.format(format, x(i)) + " + " + String.format(format, x(i + 1)) + 
          "i")
        out.newLine()
        i = i + 2
      }
      out.newLine()
      out.close()
    } catch {
      case e: IOException => e.printStackTrace()
    }
  }

  /**
   * Saves elements of <code>x</code> in a file <code>filename</code>,
   * assuming that it is 2D complex array. Complex data is represented by 2
   * double values in sequence: the real and imaginary parts.
   *
   * @param n1
   * @param n2
   * @param x
   * @param filename
   */
  def writeToFileComplex_2D(format: String, 
      n1: Int, 
      n2: Int, 
      x: Array[Double], 
      filename: String) {
    try {
      val out = new BufferedWriter(new FileWriter(filename))
      for (i <- 0 until n1) {
        var j = 0
        while (j < 2 * n2) {
          if ((Math.abs(x(i * 2 * n2 + j)) < 5e-5) && (Math.abs(x(i * 2 * n2 + j + 1)) < 5e-5)) {
            if (x(i * 2 * n2 + j + 1) >= 0.0) {
              out.write("0 + 0i\t")
            } else {
              out.write("0 - 0i\t")
            }
            //continue
          }
          if (Math.abs(x(i * 2 * n2 + j + 1)) < 5e-5) {
            if (x(i * 2 * n2 + j + 1) >= 0.0) {
              out.write(String.format(format, x(i * 2 * n2 + j)) + " + 0i\t")
            } else {
              out.write(String.format(format, x(i * 2 * n2 + j)) + " - 0i\t")
            }
            //continue
          }
          if (Math.abs(x(i * 2 * n2 + j)) < 5e-5) {
            if (x(i * 2 * n2 + j + 1) >= 0.0) {
              out.write("0 + " + String.format(format, x(i * 2 * n2 + j + 1)) + 
                "i\t")
            } else {
              out.write("0 - " + String.format(format, -x(i * 2 * n2 + j + 1)) + 
                "i\t")
            }
            //continue
          }
          if (x(i * 2 * n2 + j + 1) < 0) {
            out.write(String.format(format, x(i * 2 * n2 + j)) + " - " + String.format(format, -x(i * 2 * n2 + j + 1)) + 
              "i\t")
            //continue
          }
          out.write(String.format(format, x(i * 2 * n2 + j)) + " + " + String.format(format, x(i * 2 * n2 + j + 1)) + 
            "i\t")
          j = j + 2
        }
        out.newLine()
      }
      out.newLine()
      out.close()
    } catch {
      case e: IOException => e.printStackTrace()
    }
  }

  /**
   * Saves elements of <code>x</code> in a file <code>filename</code>. Complex
   * data is represented by 2 double values in sequence: the real and
   * imaginary parts.
   *
   * @param n1
   * @param n2
   * @param x
   * @param filename
   */
  def writeToFileComplex_2D(format: String, 
      n1: Int, 
      n2: Int, 
      x: Array[Array[Double]], 
      filename: String) {
    try {
      val out = new BufferedWriter(new FileWriter(filename))
      for (i <- 0 until n1) {
        var j = 0
        while (j < 2 * n2) {
          if ((Math.abs(x(i)(j)) < 5e-5) && (Math.abs(x(i)(j + 1)) < 5e-5)) {
            if (x(i)(j + 1) >= 0.0) {
              out.write("0 + 0i\t")
            } else {
              out.write("0 - 0i\t")
            }
            //continue
          }
          if (Math.abs(x(i)(j + 1)) < 5e-5) {
            if (x(i)(j + 1) >= 0.0) {
              out.write(String.format(format, x(i)(j)) + " + 0i\t")
            } else {
              out.write(String.format(format, x(i)(j)) + " - 0i\t")
            }
            //continue
          }
          if (Math.abs(x(i)(j)) < 5e-5) {
            if (x(i)(j + 1) >= 0.0) {
              out.write("0 + " + String.format(format, x(i)(j + 1)) + "i\t")
            } else {
              out.write("0 - " + String.format(format, -x(i)(j + 1)) + "i\t")
            }
            //continue
          }
          if (x(i)(j + 1) < 0) {
            out.write(String.format(format, x(i)(j)) + " - " + String.format(format, -x(i)(j + 1)) + 
              "i\t")
            //continue
          }
          out.write(String.format(format, x(i)(j)) + " + " + String.format(format, x(i)(j + 1)) + 
            "i\t")
          j = j + 2
        }
        out.newLine()
      }
      out.newLine()
      out.close()
    } catch {
      case e: IOException => e.printStackTrace()
    }
  }

  /**
   * Saves elements of <code>x</code> in a file <code>filename</code>,
   * assuming that it is 3D complex array. Complex data is represented by 2
   * double values in sequence: the real and imaginary parts.
   *
   * @param n1
   * @param n2
   * @param n3
   * @param x
   * @param filename
   */
  def writeToFileComplex_3D(format: String, 
      n1: Int, 
      n2: Int, 
      n3: Int, 
      x: Array[Double], 
      filename: String) {
    val sliceStride = n2 * n3 * 2
    val rowStride = n3 * 2
    try {
      val out = new BufferedWriter(new FileWriter(filename))
      var k = 0
      while (k < 2 * n3) {
        out.newLine()
        out.write("(:,:," + k / 2 + ")=")
        out.newLine()
        out.newLine()
        for (i <- 0 until n1) {
          for (j <- 0 until n2) {
            if (x(i * sliceStride + j * rowStride + k + 1) == 0) {
              out.write(String.format(format, x(i * sliceStride + j * rowStride + k)) + 
                "\t")
              //continue
            }
            if (x(i * sliceStride + j * rowStride + k) == 0) {
              out.write(String.format(format, x(i * sliceStride + j * rowStride + k + 1)) + 
                "i\t")
              //continue
            }
            if (x(i * sliceStride + j * rowStride + k + 1) < 0) {
              out.write(String.format(format, x(i * sliceStride + j * rowStride + k)) + 
                " - " + 
                String.format(format, -x(i * sliceStride + j * rowStride + k + 1)) + 
                "i\t")
              //continue
            }
            out.write(String.format(format, x(i * sliceStride + j * rowStride + k)) + 
              " + " + 
              String.format(format, x(i * sliceStride + j * rowStride + k + 1)) + 
              "i\t")
          }
          out.newLine()
        }
        k = k + 2
      }
      out.newLine()
      out.close()
    } catch {
      case e: IOException => e.printStackTrace()
    }
  }

  /**
   * Saves elements of <code>x</code> in a file <code>filename</code>. Complex
   * data is represented by 2 double values in sequence: the real and
   * imaginary parts.
   *
   * @param n1
   * @param n2
   * @param n3
   * @param x
   * @param filename
   */
  def writeToFileComplex_3D(format: String, 
      n1: Int, 
      n2: Int, 
      n3: Int, 
      x: Array[Array[Array[Double]]], 
      filename: String) {
    try {
      val out = new BufferedWriter(new FileWriter(filename))
      var k = 0
      while (k < 2 * n3) {
        out.newLine()
        out.write("(:,:," + k / 2 + ")=")
        out.newLine()
        out.newLine()
        for (i <- 0 until n1) {
          for (j <- 0 until n2) {
            if (x(i)(j)(k + 1) == 0) {
              out.write(String.format(format, x(i)(j)(k)) + "\t")
              //continue
            }
            if (x(i)(j)(k) == 0) {
              out.write(String.format(format, x(i)(j)(k + 1)) + "i\t")
              //continue
            }
            if (x(i)(j)(k + 1) < 0) {
              out.write(String.format(format, x(i)(j)(k)) + " - " + String.format(format, -x(i)(j)(k + 1)) + 
                "i\t")
              //continue
            }
            out.write(String.format(format, x(i)(j)(k)) + " + " + String.format(format, x(i)(j)(k + 1)) + 
              "i\t")
          }
          out.newLine()
        }
        k = k + 2
      }
      out.newLine()
      out.close()
    } catch {
      case e: IOException => e.printStackTrace()
    }
  }

  /**
   * Saves elements of <code>x</code> in a file <code>filename</code>,
   * assuming that it is 2D real array.
   *
   * @param x
   * @param filename
   */
  def writeToFileReal_1D(format: String, x: Array[Double], filename: String) {
    try {
      val out = new BufferedWriter(new FileWriter(filename))
      for (j <- 0 until x.length) {
        out.write(String.format(format, x(j)))
        out.newLine()
      }
      out.close()
    } catch {
      case e: IOException => e.printStackTrace()
    }
  }

  /**
   * Saves elements of <code>x</code> in a file <code>filename</code>,
   * assuming that it is 2D real array.
   *
   * @param x
   * @param filename
   */
  def writeToFileReal_1D(x: Array[Int], filename: String) {
    try {
      val out = new BufferedWriter(new FileWriter(filename))
      for (j <- 0 until x.length) {
        out.write(Integer toString x(j))
        out.newLine()
      }
      out.close()
    } catch {
      case e: IOException => e.printStackTrace()
    }
  }

  /**
   * Saves elements of <code>x</code> in a file <code>filename</code>,
   * assuming that it is 2D real array.
   *
   * @param n1
   * @param n2
   * @param x
   * @param filename
   */
  def writeToFileReal_2D(format: String, 
      n1: Int, 
      n2: Int, 
      x: Array[Double], 
      filename: String) {
    try {
      val out = new BufferedWriter(new FileWriter(filename))
      for (i <- 0 until n1) {
        for (j <- 0 until n2) {
          out.write(String.format(format, x(i * n2 + j)) + "\t")
        }
        out.newLine()
      }
      out.close()
    } catch {
      case e: IOException => e.printStackTrace()
    }
  }

  /**
   * Saves elements of <code>x</code> in a file <code>filename</code>,
   *
   * @param x
   * @param filename
   */
  def writeToFileReal_2D(format: String, x: Array[Array[Double]], filename: String) {
    try {
      val out = new BufferedWriter(new FileWriter(filename))
      for (i <- 0 until x.length) {
        for (j <- 0 until x(0).length) {
          out.write(String.format(format, x(i)(j)) + "\t")
        }
        out.newLine()
      }
      out.close()
    } catch {
      case e: IOException => e.printStackTrace()
    }
  }

  /**
   * Saves elements of <code>x</code> in a file <code>filename</code>,
   *
   * @param x
   * @param filename
   */
  def writeToFileReal_2D(format: String, x: Array[Array[Float]], filename: String) {
    try {
      val out = new BufferedWriter(new FileWriter(filename))
      for (i <- 0 until x.length) {
        for (j <- 0 until x(0).length) {
          out.write(String.format(format, x(i)(j)) + "\t")
        }
        out.newLine()
      }
      out.close()
    } catch {
      case e: IOException => e.printStackTrace()
    }
  }

  /**
   * Saves elements of <code>x</code> in a file <code>filename</code>,
   * assuming that it is 3D real array.
   *
   * @param slices
   * @param rows
   * @param columns
   * @param x
   * @param filename
   */
  def writeToFileReal_3D(format: String, 
      slices: Int, 
      rows: Int, 
      columns: Int, 
      x: Array[Double], 
      filename: String) {
    val sliceStride = rows * columns
    val rowStride = columns
    try {
      val out = new BufferedWriter(new FileWriter(filename))
      for (s <- 0 until slices) {
        out.newLine()
        out.write("(" + s + ",:,:)=")
        out.newLine()
        out.newLine()
        for (r <- 0 until rows) {
          for (c <- 0 until columns) {
            out.write(String.format(format, x(s * sliceStride + r * rowStride + c)) + 
              "\t")
          }
          out.newLine()
        }
        out.newLine()
      }
      out.close()
    } catch {
      case e: IOException => e.printStackTrace()
    }
  }

  /**
   * Saves elements of <code>x</code> in a file <code>filename</code>,
   * assuming that it is 2D real array.
   *
   * @param x
   * @param filename
   */
  def writeToFileReal_1D(format: String, x: Array[Float], filename: String) {
    try {
      val out = new BufferedWriter(new FileWriter(filename))
      for (j <- 0 until x.length) {
        out.write(String.format(format, x(j)))
        out.newLine()
      }
      out.close()
    } catch {
      case e: IOException => e.printStackTrace()
    }
  }

  /**
   * Saves elements of <code>x</code> in a file <code>filename</code>,
   * assuming that it is 2D real array.
   *
   * @param n1
   * @param n2
   * @param x
   * @param filename
   */
  def writeToFileReal_2D(format: String, 
      n1: Int, 
      n2: Int, 
      x: Array[Float], 
      filename: String) {
    try {
      val out = new BufferedWriter(new FileWriter(filename))
      for (i <- 0 until n1) {
        for (j <- 0 until n2) {
          if (Math.abs(x(i * n2 + j)) < 5e-5) {
            out.write("0\t")
          } else {
            out.write(String.format(format, x(i * n2 + j)) + "\t")
          }
        }
        out.newLine()
      }
      out.close()
    } catch {
      case e: IOException => e.printStackTrace()
    }
  }

  /**
   * Saves elements of <code>x</code> in a file <code>filename</code>,
   * assuming that it is 3D real array.
   *
   * @param n1
   * @param n2
   * @param n3
   * @param x
   * @param filename
   */
  def writeToFileReal_3D(format: String, 
      n1: Int, 
      n2: Int, 
      n3: Int, 
      x: Array[Float], 
      filename: String) {
    val sliceStride = n2 * n3
    val rowStride = n3
    try {
      val out = new BufferedWriter(new FileWriter(filename))
      for (k <- 0 until n3) {
        out.newLine()
        out.write("(:,:," + k + ")=")
        out.newLine()
        out.newLine()
        for (i <- 0 until n1) {
          for (j <- 0 until n2) {
            out.write(String.format(format, x(i * sliceStride + j * rowStride + k)) + 
              "\t")
          }
          out.newLine()
        }
        out.newLine()
      }
      out.close()
    } catch {
      case e: IOException => e.printStackTrace()
    }
  }

  /**
   * Saves results of the benchmark in a file
   *
   * @param filename
   * @param nthread
   * @param niter
   * @param doWarmup
   * @param doScaling
   * @param times
   * @param sizes
   */
  def writeFFTBenchmarkResultsToFile(filename: String, 
      nthread: Int, 
      niter: Int, 
      doWarmup: Boolean, 
      doScaling: Boolean, 
      sizes: Array[Int], 
      times: Array[Double]) {
    val properties = Array("os.name", "os.version", "os.arch", "java.vendor", "java.version")
    try {
      val out = new BufferedWriter(new FileWriter(filename, false))
      out.write(new Date().toString)
      out.newLine()
      out.write("System properties:")
      out.newLine()
      out.write("\tos.name = " + System.getProperty(properties(0)))
      out.newLine()
      out.write("\tos.version = " + System.getProperty(properties(1)))
      out.newLine()
      out.write("\tos.arch = " + System.getProperty(properties(2)))
      out.newLine()
      out.write("\tjava.vendor = " + System.getProperty(properties(3)))
      out.newLine()
      out.write("\tjava.version = " + System.getProperty(properties(4)))
      out.newLine()
      out.write("\tavailable processors = " + Runtime.getRuntime.availableProcessors())
      out.newLine()
      out.write("Settings:")
      out.newLine()
      out.write("\tused processors = " + nthread)
      out.newLine()
      out.write("\tTHREADS_BEGIN_N_2D = " + ConcurrencyUtils.getThreadsBeginN_2D)
      out.newLine()
      out.write("\tTHREADS_BEGIN_N_3D = " + ConcurrencyUtils.getThreadsBeginN_3D)
      out.newLine()
      out.write("\tnumber of iterations = " + niter)
      out.newLine()
      out.write("\twarm-up performed = " + doWarmup)
      out.newLine()
      out.write("\tscaling performed = " + doScaling)
      out.newLine()
      out.write("--------------------------------------------------------------------------------------------------")
      out.newLine()
      out.write("sizes=[")
      for (i <- 0 until sizes.length) {
        out.write(Integer toString sizes(i))
        if (i < sizes.length - 1) {
          out.write(", ")
        } else {
          out.write("]")
        }
      }
      out.newLine()
      out.write("times(in msec)=[")
      for (i <- 0 until times.length) {
        out.write(String.format("%.2f", times(i)))
        if (i < times.length - 1) {
          out.write(", ")
        } else {
          out.write("]")
        }
      }
      out.newLine()
      out.close()
    } catch {
      case e: IOException => e.printStackTrace()
    }
  }
}
