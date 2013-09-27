package cern.colt.matrix.tdouble.algo

import cern.colt.matrix.AbstractFormatter
import cern.colt.matrix.AbstractMatrix1D
import cern.colt.matrix.AbstractMatrix2D
import cern.colt.matrix.Formatter
import cern.colt.matrix.tdouble.StrideMatrix1D
import cern.colt.matrix.tdouble.StrideMatrix2D
import cern.colt.matrix.tdouble.DoubleMatrix3D
import cern.colt.matrix.tdouble.impl.DenseMatrix1D
import DoubleFormatter._
//remove if not needed
import scala.collection.JavaConversions._

object DoubleFormatter {

  /**
   * Demonstrates how to use this class.
   */
  def demo1() {
    val values = Array(Array(3, 0, -3.4, 0), Array(5.1, 0, +3.0123456789, 0), Array(16.37, 0.0, 2.5, 0), Array(-16.3, 0, -3.012345678E-4, -1), Array(1236.3456789, 0, 7, -1.2))
    val formats = Array("%G", "%1.10G", "%f", "%1.2f", "%0.2e", null)
    val size = formats.length
    val matrix = cern.colt.matrix.tdouble.DoubleFactory2D.dense.make(values)
    val strings = Array.ofDim[String](size)
    val sourceCodes = Array.ofDim[String](size)
    for (i <- 0 until size) {
      val format = formats(i)
      strings(i) = new DoubleFormatter(format) toString matrix
      sourceCodes(i) = new DoubleFormatter(format).toSourceCode(matrix)
    }
    println("original:\n" + new DoubleFormatter().toString(matrix))
    for (i <- 0 until size) {
      println("\nstring(" + formats(i) + "):\n" + strings(i))
      println("\nsourceCode(" + formats(i) + "):\n" + sourceCodes(i))
    }
  }

  /**
   * Demonstrates how to use this class.
   */
  def demo2() {
    val values = Array(5, 0.0, -0.0, -Double.NaN, Double.NaN, 0.0 / 0.0, Double.MinValue, Double.MaxValue, Double.NEGATIVE_INFINITY, Double.POSITIVE_INFINITY)
    val formats = Array("%G", "%1.19G")
    val size = formats.length
    val matrix = new DenseMatrix1D(values)
    val strings = Array.ofDim[String](size)
    for (i <- 0 until size) {
      val format = formats(i)
      strings(i) = new DoubleFormatter(format) toString matrix
      for (j <- 0 until matrix.size) {
        println(String.valueOf(matrix.get(j)))
      }
    }
    println("original:\n" + new DoubleFormatter() toString matrix)
    for (i <- 0 until size) {
      println("\nstring(" + formats(i) + "):\n" + strings(i))
    }
  }

  /**
   * Demonstrates how to use this class.
   */
  def demo3(size: Int, value: Double) {
    val timer = new cern.colt.Timer()
    var s: String = null
    var buf: StringBuffer = null
    val matrix = cern.colt.matrix.tdouble.DoubleFactory2D.dense.make(size, size, value)
    timer.reset().start()
    buf = new StringBuffer()
    var i = size
    while (i >= 0) {
      var j = size
      while (j >= 0) {
        buf.append(matrix.getQuick(i, j))
        j -= 1
      }
      i -= 1
    }
    buf = null
    timer.stop().display()
    timer.reset().start()
    val format = new cern.colt.matrix.FormatterFactory().create("%G")
    buf = new StringBuffer()
    var i = size
    while (i >= 0) {
      var j = size
      while (j >= 0) {
        buf.append(format.form(matrix.getQuick(i, j)))
      }
    }
    buf = null
    timer.stop().display()
    timer.reset().start()
    s = new DoubleFormatter(null) toString matrix
    println(s)
    s = null
    timer.stop().display()
    timer.reset().start()
    s = new DoubleFormatter("%G") toString matrix
    println(s)
    s = null
    timer.stop().display()
  }

  /**
   * Demonstrates how to use this class.
   */
  def demo4() {
    val values = Array(Array(3, 0, -3.4, 0), Array(5.1, 0, +3.0123456789, 0), Array(16.37, 0.0, 2.5, 0), Array(-16.3, 0, -3.012345678E-4, -1), Array(1236.3456789, 0, 7, -1.2))
    val columnNames = Array("0.1", "0.3", "0.5", "0.7")
    val rowNames = Array("SunJDK1.2.2 classic", "IBMJDK1.1.8", "SunJDK1.3 Hotspot", "other1", "other2")
    val matrix = cern.colt.matrix.tdouble.DoubleFactory2D.dense.make(values)
    println("\n\n" +
      new DoubleFormatter("%G").toTitleString(matrix, rowNames, columnNames, "rowAxis", "colAxis", "VM Performance: Provider vs. matrix density"))
  }

  /**
   * Demonstrates how to use this class.
   */
  def demo5() {
    val values = Array(Array(3, 0, -3.4, 0), Array(5.1, 0, +3.0123456789, 0), Array(16.37, 0.0, 2.5, 0), Array(-16.3, 0, -3.012345678E-4, -1), Array(1236.3456789, 0, 7, -1.2))
    val columnNames = Array("0.1", "0.3", "0.5", "0.7")
    val rowNames = Array("SunJDK1.2.2 classic", "IBMJDK1.1.8", "SunJDK1.3 Hotspot", "other1", "other2")
    println(cern.colt.matrix.tdouble.DoubleFactory2D.dense.make(values))
    println(new DoubleFormatter("%G").toTitleString(cern.colt.matrix.tdouble.DoubleFactory2D.dense.make(values),
      rowNames, columnNames, "vendor", "density", "title"))
  }

  /**
   * Demonstrates how to use this class.
   */
  def demo6() {
    val values = Array(Array(3, 0, -3.4, 0), Array(5.1, 0, +3.0123456789, 0), Array(16.37, 0.0, 2.5, 0), Array(-16.3, 0, -3.012345678E-4, -1), Array(1236.3456789, 0, 7, -1.2))
    val columnNames = Array("W", "X", "Y", "Z")
    val rowNames = Array("SunJDK1.2.2 classic", "IBMJDK1.1.8", "SunJDK1.3 Hotspot", "other1", "other2")
    println(new DoubleFormatter() toString cern.colt.matrix.tdouble.DoubleFactory2D.dense.make(values))
    println(new DoubleFormatter().toTitleString(cern.colt.matrix.tdouble.DoubleFactory2D.dense.make(values),
      rowNames, columnNames, "vendor", "density", "title"))
  }

  /**
   * Demonstrates how to use this class.
   */
  def demo7() {
    val values = Array(Array(5, 10, 20, 40), Array(7, 8, 6, 7), Array(12, 10, 20, 19), Array(3, 1, 5, 6))
    val columnNames = Array("1996", "1997", "1998", "1999")
    val rowNames = Array("PowerBar", "Benzol", "Mercedes", "Sparcling")
    val rowAxisName = "CPU"
    val columnAxisName = "Year"
    val title = "CPU performance over time [nops/sec]"
    val F = hep.aida.tdouble.bin.DoubleBinFunctions1D.functions
    val aggr = Array(F.mean, F.rms, F.quantile(0.25), F.median, F.quantile(0.75), F.stdDev, F.min, F.max)
    val format = "%1.2G"
    println(new DoubleFormatter(format).toTitleString(cern.colt.matrix.tdouble.DoubleFactory2D.dense.make(values),
      rowNames, columnNames, rowAxisName, columnAxisName, title, aggr))
  }
}

/**
 * Flexible, well human readable matrix print formatting; By default decimal
 * point aligned. Currenly works on 1-d, 2-d and 3-d matrices. Note that in most
 * cases you will not need to get familiar with this class; just call
 * <tt>matrix.toString()</tt> and be happy with the default formatting. This
 * class is for advanced requirements.
 * <p>
 *
 * <b>Examples:</b>
 * <p>
 * Examples demonstrate usage on 2-d matrices. 1-d and 3-d matrices formatting
 * works very similar.
 * <table border="1" cellspacing="0">
 * <tr align="center">
 * <td>Original matrix</td>
 * </tr>
 * <tr>
 * <td>
 *
 * <p>
 * <tt>double[][] values = {<br>
 {3, 0, -3.4, 0},<br>
 {5.1 ,0, +3.0123456789, 0}, <br>
 {16.37, 0.0, 2.5, 0}, <br>
 {-16.3, 0, -3.012345678E-4, -1},<br>
 {1236.3456789, 0, 7, -1.2}<br>
 };<br>
 matrix = new DenseDoubleMatrix2D(values);</tt>
 * </p>
 * </td>
 * </tr>
 * </table>
 * <p>
 * &nbsp;
 * </p>
 * <table border="1" cellspacing="0">
 * <tr align="center">
 * <td><tt>format</tt></td>
 * <td valign="top"><tt>Formatter.toString(matrix);</tt></td>
 * <td valign="top"><tt>Formatter.toSourceCode(matrix);</tt></td>
 * </tr>
 * <tr>
 * <td><tt>%G </tt><br>
 * (default)</td>
 * <td align="left" valign="top"><tt>5&nbsp;x&nbsp;4&nbsp;matrix<br>
 &nbsp;&nbsp;&nbsp;3&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;0&nbsp;-3.4&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;0&nbsp;&nbsp;<br>
 &nbsp;&nbsp;&nbsp;5.1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;0&nbsp;&nbsp;3.012346&nbsp;&nbsp;0&nbsp;&nbsp;<br>
 &nbsp;&nbsp;16.37&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;0&nbsp;&nbsp;2.5&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;0&nbsp;&nbsp;<br>
 &nbsp;-16.3&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;0&nbsp;-0.000301&nbsp;-1&nbsp;&nbsp;<br>
 1236.345679&nbsp;0&nbsp;&nbsp;7&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;-1.2
 </tt></td>
 * <td align="left" valign="top"><tt>{<br>
 &nbsp;&nbsp;&nbsp;{&nbsp;&nbsp;&nbsp;3&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;,&nbsp;0,&nbsp;-3.4&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;,&nbsp;&nbsp;0&nbsp;&nbsp;},<br>
 &nbsp;&nbsp;&nbsp;{&nbsp;&nbsp;&nbsp;5.1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;,&nbsp;0,&nbsp;&nbsp;3.012346,&nbsp;&nbsp;0&nbsp;&nbsp;},<br>
 &nbsp;&nbsp;&nbsp;{&nbsp;&nbsp;16.37&nbsp;&nbsp;&nbsp;&nbsp;,&nbsp;0,&nbsp;&nbsp;2.5&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;,&nbsp;&nbsp;0&nbsp;&nbsp;},<br>
 &nbsp;&nbsp;&nbsp;{&nbsp;-16.3&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;,&nbsp;0,&nbsp;-0.000301,&nbsp;-1&nbsp;&nbsp;},<br>
 &nbsp;&nbsp;&nbsp;{1236.345679,&nbsp;0,&nbsp;&nbsp;7&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;,&nbsp;-1.2}<br>
 }; </tt></td>
 * </tr>
 * <tr>
 * <td><tt>%1.10G</tt></td>
 * <td align="left" valign="top"><tt>5&nbsp;x&nbsp;4&nbsp;matrix<br>
 &nbsp;&nbsp;&nbsp;3&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;0&nbsp;-3.4&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;0&nbsp;&nbsp;<br>
 &nbsp;&nbsp;&nbsp;5.1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;0&nbsp;&nbsp;3.0123456789&nbsp;&nbsp;0&nbsp;&nbsp;<br>
 &nbsp;&nbsp;16.37&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;0&nbsp;&nbsp;2.5&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;0&nbsp;&nbsp;<br>
 &nbsp;-16.3&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;0&nbsp;-0.0003012346&nbsp;-1&nbsp;&nbsp;<br>
 1236.3456789&nbsp;0&nbsp;&nbsp;7&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;-1.2
 </tt></td>
 * <td align="left" valign="top"><tt>{<br>
 &nbsp;&nbsp;&nbsp;{&nbsp;&nbsp;&nbsp;3&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;,&nbsp;0,&nbsp;-3.4&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;,&nbsp;&nbsp;0&nbsp;&nbsp;},<br>
 &nbsp;&nbsp;&nbsp;{&nbsp;&nbsp;&nbsp;5.1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;,&nbsp;0,&nbsp;&nbsp;3.0123456789,&nbsp;&nbsp;0&nbsp;&nbsp;},<br>
 &nbsp;&nbsp;&nbsp;{&nbsp;&nbsp;16.37&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;,&nbsp;0,&nbsp;&nbsp;2.5&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;,&nbsp;&nbsp;0&nbsp;&nbsp;},<br>
 &nbsp;&nbsp;&nbsp;{&nbsp;-16.3&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;,&nbsp;0,&nbsp;-0.0003012346,&nbsp;-1&nbsp;&nbsp;},<br>
 &nbsp;&nbsp;&nbsp;{1236.3456789,&nbsp;0,&nbsp;&nbsp;7&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;,&nbsp;-1.2}<br>
 }; </tt></td>
 * </tr>
 * <tr>
 * <td><tt>%f</tt></td>
 * <td align="left" valign="top"> <tt> 5&nbsp;x&nbsp;4&nbsp;matrix<br>
 &nbsp;&nbsp;&nbsp;3.000000&nbsp;0.000000&nbsp;-3.400000&nbsp;&nbsp;0.000000<br>
 &nbsp;&nbsp;&nbsp;5.100000&nbsp;0.000000&nbsp;&nbsp;3.012346&nbsp;&nbsp;0.000000<br>
 &nbsp;&nbsp;16.370000&nbsp;0.000000&nbsp;&nbsp;2.500000&nbsp;&nbsp;0.000000<br>
 &nbsp;-16.300000&nbsp;0.000000&nbsp;-0.000301&nbsp;-1.000000<br>
 1236.345679&nbsp;0.000000&nbsp;&nbsp;7.000000&nbsp;-1.200000 </tt></td>
 * <td align="left" valign="top"><tt> {<br>
 &nbsp;&nbsp;&nbsp;{&nbsp;&nbsp;&nbsp;3.000000,&nbsp;0.000000,&nbsp;-3.400000,&nbsp;&nbsp;0.000000},<br>
 &nbsp;&nbsp;&nbsp;{&nbsp;&nbsp;&nbsp;5.100000,&nbsp;0.000000,&nbsp;&nbsp;3.012346,&nbsp;&nbsp;0.000000},<br>
 &nbsp;&nbsp;&nbsp;{&nbsp;&nbsp;16.370000,&nbsp;0.000000,&nbsp;&nbsp;2.500000,&nbsp;&nbsp;0.000000},<br>
 &nbsp;&nbsp;&nbsp;{&nbsp;-16.300000,&nbsp;0.000000,&nbsp;-0.000301,&nbsp;-1.000000},<br>
 &nbsp;&nbsp;&nbsp;{1236.345679,&nbsp;0.000000,&nbsp;&nbsp;7.000000,&nbsp;-1.200000}<br>
 }; </tt></td>
 * </tr>
 * <tr>
 * <td><tt>%1.2f</tt></td>
 * <td align="left" valign="top"><tt>5&nbsp;x&nbsp;4&nbsp;matrix<br>
 &nbsp;&nbsp;&nbsp;3.00&nbsp;0.00&nbsp;-3.40&nbsp;&nbsp;0.00<br>
 &nbsp;&nbsp;&nbsp;5.10&nbsp;0.00&nbsp;&nbsp;3.01&nbsp;&nbsp;0.00<br>
 &nbsp;&nbsp;16.37&nbsp;0.00&nbsp;&nbsp;2.50&nbsp;&nbsp;0.00<br>
 &nbsp;-16.30&nbsp;0.00&nbsp;-0.00&nbsp;-1.00<br>
 1236.35&nbsp;0.00&nbsp;&nbsp;7.00&nbsp;-1.20 </tt></td>
 * <td align="left" valign="top"><tt>{<br>
 &nbsp;&nbsp;&nbsp;{&nbsp;&nbsp;&nbsp;3.00,&nbsp;0.00,&nbsp;-3.40,&nbsp;&nbsp;0.00},<br>
 &nbsp;&nbsp;&nbsp;{&nbsp;&nbsp;&nbsp;5.10,&nbsp;0.00,&nbsp;&nbsp;3.01,&nbsp;&nbsp;0.00},<br>
 &nbsp;&nbsp;&nbsp;{&nbsp;&nbsp;16.37,&nbsp;0.00,&nbsp;&nbsp;2.50,&nbsp;&nbsp;0.00},<br>
 &nbsp;&nbsp;&nbsp;{&nbsp;-16.30,&nbsp;0.00,&nbsp;-0.00,&nbsp;-1.00},<br>
 &nbsp;&nbsp;&nbsp;{1236.35,&nbsp;0.00,&nbsp;&nbsp;7.00,&nbsp;-1.20}<br>
 }; </tt></td>
 * </tr>
 * <tr>
 * <td><tt>%0.2e</tt></td>
 * <td align="left" valign="top"><tt>5&nbsp;x&nbsp;4&nbsp;matrix<br>
 &nbsp;3.00e+000&nbsp;0.00e+000&nbsp;-3.40e+000&nbsp;&nbsp;0.00e+000<br>
 &nbsp;5.10e+000&nbsp;0.00e+000&nbsp;&nbsp;3.01e+000&nbsp;&nbsp;0.00e+000<br>
 &nbsp;1.64e+001&nbsp;0.00e+000&nbsp;&nbsp;2.50e+000&nbsp;&nbsp;0.00e+000<br>
 -1.63e+001&nbsp;0.00e+000&nbsp;-3.01e-004&nbsp;-1.00e+000<br>
 &nbsp;1.24e+003&nbsp;0.00e+000&nbsp;&nbsp;7.00e+000&nbsp;-1.20e+000 </tt></td>
 * <td align="left" valign="top"><tt>{<br>
 &nbsp;&nbsp;&nbsp;{&nbsp;3.00e+000,&nbsp;0.00e+000,&nbsp;-3.40e+000,&nbsp;&nbsp;0.00e+000},<br>
 &nbsp;&nbsp;&nbsp;{&nbsp;5.10e+000,&nbsp;0.00e+000,&nbsp;&nbsp;3.01e+000,&nbsp;&nbsp;0.00e+000},<br>
 &nbsp;&nbsp;&nbsp;{&nbsp;1.64e+001,&nbsp;0.00e+000,&nbsp;&nbsp;2.50e+000,&nbsp;&nbsp;0.00e+000},<br>
 &nbsp;&nbsp;&nbsp;{-1.63e+001,&nbsp;0.00e+000,&nbsp;-3.01e-004,&nbsp;-1.00e+000},<br>
 &nbsp;&nbsp;&nbsp;{&nbsp;1.24e+003,&nbsp;0.00e+000,&nbsp;&nbsp;7.00e+000,&nbsp;-1.20e+000}<br>
 }; </tt></td>
 * </tr>
 * <tr>
 * <td><tt>null</tt></td>
 * <td align="left" valign="top"><tt>5&nbsp;x&nbsp;4&nbsp;matrix <br>
 &nbsp;&nbsp;&nbsp;3.0&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;0.0&nbsp;-3.4&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;0.0<br>
 &nbsp;&nbsp;&nbsp;5.1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;0.0&nbsp;&nbsp;3.0123456789&nbsp;&nbsp;&nbsp;&nbsp;0.0<br>
 &nbsp;&nbsp;16.37&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;0.0&nbsp;&nbsp;2.5&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;0.0<br>
 &nbsp;-16.3&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;0.0&nbsp;-3.012345678E-4&nbsp;-1.0<br>
 1236.3456789&nbsp;0.0&nbsp;&nbsp;7.0&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;-1.2
 </tt> <tt> </tt></td>
 * <td align="left" valign="top"><tt> {<br>
 &nbsp;&nbsp;&nbsp;{&nbsp;&nbsp;&nbsp;3.0&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;,&nbsp;0.0,&nbsp;-3.4&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;,&nbsp;&nbsp;0.0},<br>
 &nbsp;&nbsp;&nbsp;{&nbsp;&nbsp;&nbsp;5.1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;,&nbsp;0.0,&nbsp;&nbsp;3.0123456789&nbsp;&nbsp;,&nbsp;&nbsp;0.0},<br>
 &nbsp;&nbsp;&nbsp;{&nbsp;&nbsp;16.37&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;,&nbsp;0.0,&nbsp;&nbsp;2.5&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;,&nbsp;&nbsp;0.0},<br>
 &nbsp;&nbsp;&nbsp;{&nbsp;-16.3&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;,&nbsp;0.0,&nbsp;-3.012345678E-4,&nbsp;-1.0},<br>
 &nbsp;&nbsp;&nbsp;{1236.3456789,&nbsp;0.0,&nbsp;&nbsp;7.0&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;,&nbsp;-1.2}<br>
 }; </tt></td>
 * </tr>
 * </table>
 *
 * <p>
 * Here are some more elaborate examples, adding labels for axes, rows, columns,
 * title and some statistical aggregations.
 * </p>
 * <table border="1" cellspacing="0">
 * <tr>
 * <td nowrap>
 * <p>
 * <tt> double[][] values = {<br>
 {5 ,10, 20, 40 },<br>
 { 7, 8 , 6 , 7 },<br>
 {12 ,10, 20, 19 },<br>
 { 3, 1 , 5 , 6 }<br>
 }; <br>
 </tt><tt>String title = "CPU performance over time [nops/sec]";<br>
 String columnAxisName = "Year";<br>
 String rowAxisName = "CPU"; <br>
 String[] columnNames = {"1996", "1997", "1998", "1999"};<br>
 String[] rowNames = { "PowerBar", "Benzol", "Mercedes", "Sparcling"};<br>
 hep.aida.bin.BinFunctions1D F = hep.aida.bin.BinFunctions1D.functions; // alias<br>
 hep.aida.bin.BinFunction1D[] aggr = {F.mean, F.rms, F.quantile(0.25), F.median, F.quantile(0.75), F.stdDev, F.min, F.max};<br>
 String format = "%1.2G";<br>
 DoubleMatrix2D matrix = new DenseDoubleMatrix2D(values); <br>
 new Formatter(format).toTitleString(<br>
 &nbsp;&nbsp;&nbsp;matrix,rowNames,columnNames,rowAxisName,columnAxisName,title,aggr); </tt>
 * </p>
 * </td>
 * </tr>
 * <tr>
 * <td><tt>
 CPU&nbsp;performance&nbsp;over&nbsp;time&nbsp;[nops/sec]<br>
 &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;|&nbsp;Year<br>
 &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;|&nbsp;1996&nbsp;&nbsp;1997&nbsp;&nbsp;1998&nbsp;&nbsp;1999&nbsp;&nbsp;|&nbsp;Mean&nbsp;&nbsp;RMS&nbsp;&nbsp;&nbsp;25%&nbsp;Q.&nbsp;Median&nbsp;75%&nbsp;Q.&nbsp;StdDev&nbsp;Min&nbsp;Max<br>
 ---------------------------------------------------------------------------------------<br>
 C&nbsp;PowerBar&nbsp;&nbsp;|&nbsp;&nbsp;5&nbsp;&nbsp;&nbsp;&nbsp;10&nbsp;&nbsp;&nbsp;&nbsp;20&nbsp;&nbsp;&nbsp;&nbsp;40&nbsp;&nbsp;&nbsp;&nbsp;|&nbsp;18.75&nbsp;23.05&nbsp;&nbsp;8.75&nbsp;&nbsp;15&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;25&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;15.48&nbsp;&nbsp;&nbsp;5&nbsp;&nbsp;40&nbsp;<br>
 P&nbsp;Benzol&nbsp;&nbsp;&nbsp;&nbsp;|&nbsp;&nbsp;7&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;8&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;6&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;7&nbsp;&nbsp;&nbsp;&nbsp;|&nbsp;&nbsp;7&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;7.04&nbsp;&nbsp;6.75&nbsp;&nbsp;&nbsp;7&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;7.25&nbsp;&nbsp;&nbsp;0.82&nbsp;&nbsp;&nbsp;6&nbsp;&nbsp;&nbsp;8&nbsp;<br>
 U&nbsp;Mercedes&nbsp;&nbsp;|&nbsp;12&nbsp;&nbsp;&nbsp;&nbsp;10&nbsp;&nbsp;&nbsp;&nbsp;20&nbsp;&nbsp;&nbsp;&nbsp;19&nbsp;&nbsp;&nbsp;&nbsp;|&nbsp;15.25&nbsp;15.85&nbsp;11.5&nbsp;&nbsp;&nbsp;15.5&nbsp;&nbsp;&nbsp;19.25&nbsp;&nbsp;&nbsp;4.99&nbsp;&nbsp;10&nbsp;&nbsp;20&nbsp;<br>
 &nbsp;&nbsp;Sparcling&nbsp;|&nbsp;&nbsp;3&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;5&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;6&nbsp;&nbsp;&nbsp;&nbsp;|&nbsp;&nbsp;3.75&nbsp;&nbsp;4.21&nbsp;&nbsp;2.5&nbsp;&nbsp;&nbsp;&nbsp;4&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;5.25&nbsp;&nbsp;&nbsp;2.22&nbsp;&nbsp;&nbsp;1&nbsp;&nbsp;&nbsp;6&nbsp;<br>
 ---------------------------------------------------------------------------------------<br>
 &nbsp;&nbsp;Mean&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;|&nbsp;&nbsp;6.75&nbsp;&nbsp;7.25&nbsp;12.75&nbsp;18&nbsp;&nbsp;&nbsp;&nbsp;|&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<br>
 &nbsp;&nbsp;RMS&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;|&nbsp;&nbsp;7.53&nbsp;&nbsp;8.14&nbsp;14.67&nbsp;22.62&nbsp;|&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<br>
 &nbsp;&nbsp;25%&nbsp;Q.&nbsp;&nbsp;&nbsp;&nbsp;|&nbsp;&nbsp;4.5&nbsp;&nbsp;&nbsp;6.25&nbsp;&nbsp;5.75&nbsp;&nbsp;6.75&nbsp;|&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<br>
 &nbsp;&nbsp;Median&nbsp;&nbsp;&nbsp;&nbsp;|&nbsp;&nbsp;6&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;9&nbsp;&nbsp;&nbsp;&nbsp;13&nbsp;&nbsp;&nbsp;&nbsp;13&nbsp;&nbsp;&nbsp;&nbsp;|&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<br>
 &nbsp;&nbsp;75%&nbsp;Q.&nbsp;&nbsp;&nbsp;&nbsp;|&nbsp;&nbsp;8.25&nbsp;10&nbsp;&nbsp;&nbsp;&nbsp;20&nbsp;&nbsp;&nbsp;&nbsp;24.25&nbsp;|&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<br>
 &nbsp;&nbsp;StdDev&nbsp;&nbsp;&nbsp;&nbsp;|&nbsp;&nbsp;3.86&nbsp;&nbsp;4.27&nbsp;&nbsp;8.38&nbsp;15.81&nbsp;|&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<br>
 &nbsp;&nbsp;Min&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;|&nbsp;&nbsp;3&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;5&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;6&nbsp;&nbsp;&nbsp;&nbsp;|&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<br>
 &nbsp;&nbsp;Max&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;|&nbsp;12&nbsp;&nbsp;&nbsp;&nbsp;10&nbsp;&nbsp;&nbsp;&nbsp;20&nbsp;&nbsp;&nbsp;&nbsp;19&nbsp;&nbsp;&nbsp;&nbsp;|&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
 </tt></td>
 * </tr>
 * <tr>
 * <td nowrap><tt> same as above, but now without aggregations<br>
 aggr=null; </tt></td>
 * </tr>
 * <tr>
 * <td><tt> CPU&nbsp;performance&nbsp;over&nbsp;time&nbsp;[nops/sec]<br>
 &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;|&nbsp;Year<br>
 &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;|&nbsp;1996&nbsp;1997&nbsp;1998&nbsp;1999<br>
 ---------------------------------<br>
 C&nbsp;PowerBar&nbsp;&nbsp;|&nbsp;&nbsp;5&nbsp;&nbsp;&nbsp;10&nbsp;&nbsp;&nbsp;20&nbsp;&nbsp;&nbsp;40&nbsp;&nbsp;<br>
 P&nbsp;Benzol&nbsp;&nbsp;&nbsp;&nbsp;|&nbsp;&nbsp;7&nbsp;&nbsp;&nbsp;&nbsp;8&nbsp;&nbsp;&nbsp;&nbsp;6&nbsp;&nbsp;&nbsp;&nbsp;7&nbsp;&nbsp;<br>
 U&nbsp;Mercedes&nbsp;&nbsp;|&nbsp;12&nbsp;&nbsp;&nbsp;10&nbsp;&nbsp;&nbsp;20&nbsp;&nbsp;&nbsp;19&nbsp;&nbsp;<br>
 &nbsp;&nbsp;Sparcling&nbsp;|&nbsp;&nbsp;3&nbsp;&nbsp;&nbsp;&nbsp;1&nbsp;&nbsp;&nbsp;&nbsp;5&nbsp;&nbsp;&nbsp;&nbsp;6&nbsp;&nbsp;
 </tt></td>
 * </tr>
 * <tr>
 * <td nowrap>
 * <p>
 * <tt> same as above, but now without rows labeled<br>
 aggr=null;<br>
 rowNames=null;<br>
 rowAxisName=null; </tt>
 * </p>
 * </td>
 * </tr>
 * <tr>
 * <td><tt>
 CPU&nbsp;performance&nbsp;over&nbsp;time&nbsp;[nops/sec]<br>
 Year<br>
 1996&nbsp;1997&nbsp;1998&nbsp;1999<br>
 -------------------<br>
 &nbsp;5&nbsp;&nbsp;&nbsp;10&nbsp;&nbsp;&nbsp;20&nbsp;&nbsp;&nbsp;40&nbsp;&nbsp;<br>
 &nbsp;7&nbsp;&nbsp;&nbsp;&nbsp;8&nbsp;&nbsp;&nbsp;&nbsp;6&nbsp;&nbsp;&nbsp;&nbsp;7&nbsp;&nbsp;<br>
 12&nbsp;&nbsp;&nbsp;10&nbsp;&nbsp;&nbsp;20&nbsp;&nbsp;&nbsp;19&nbsp;&nbsp;<br>
 &nbsp;3&nbsp;&nbsp;&nbsp;&nbsp;1&nbsp;&nbsp;&nbsp;&nbsp;5&nbsp;&nbsp;&nbsp;&nbsp;6&nbsp;&nbsp;
 </tt></td>
 * </tr>
 * </table>
 *
 * <p>
 * A column can be broader than specified by the parameter
 * <tt>minColumnWidth</tt> (because a cell may not fit into that width) but a
 * column is never smaller than <tt>minColumnWidth</tt>. Normally one does not
 * need to specify <tt>minColumnWidth</tt> (default is <tt>1</tt>). This
 * parameter is only interesting when wanting to print two distinct matrices
 * such that both matrices have the same column width, for example, to make it
 * easier to see which column of matrix A corresponds to which column of matrix
 * B.
 * </p>
 *
 * <p>
 * <b>Implementation:</b>
 * </p>
 *
 * <p>
 * Note that this class is by no means ment to be used for high performance I/O
 * (serialization is much quicker). It is ment to produce well human readable
 * output.
 * </p>
 * <p>
 * Analyzes the entire matrix before producing output. Each cell is converted to
 * a String as indicated by the given C-like format string. If <tt>null</tt> is
 * passed as format string, {@link java.lang.Double#toString(double)} is used
 * instead, yielding full precision.
 * </p>
 * <p>
 * Next, leading and trailing whitespaces are removed. For each column the
 * maximum number of characters before and after the decimal point is
 * determined. (No problem if decimal points are missing). Each cell is then
 * padded with leading and trailing blanks, as necessary to achieve decimal
 * point aligned, left justified formatting.
 * </p>
 *
 * @author wolfgang.hoschek@cern.ch
 * @version 1.2, 11/30/99
 */
@SerialVersionUID(1L)
class DoubleFormatter(format: String) extends AbstractFormatter {

  setFormat(format)

  setAlignment(AbstractFormatter.DECIMAL)

  /**
   * Constructs and returns a matrix formatter with format <tt>"%G"</tt>.
   */
  def this() {
    this("%G")
  }

  /**
   * Converts a given cell to a String; no alignment considered.
   */
  protected def form(matrix: StrideMatrix1D, index: Int, formatter: Formatter): String = formatter.format(matrix.get(index))

  /**
   * Converts a given cell to a String; no alignment considered.
   */
  protected def form(matrix: AbstractMatrix1D, index: Int, formatter: Formatter): String = {
    this.form(matrix.asInstanceOf[StrideMatrix1D], index, formatter)
  }

  /**
   * Returns a string representations of all cells; no alignment considered.
   */
  def format(matrix: StrideMatrix2D): Array[Array[String]] = {
    val strings = Array.ofDim[String](matrix.rows(), matrix.columns())
    var row = matrix.rows()
    while (row >= 0) strings(row) = formatRow(matrix.viewRow(row))
    strings
  }

  /**
   * Returns a string representations of all cells; no alignment considered.
   */
  protected def format(matrix: AbstractMatrix2D): Array[Array[String]] = {
    this.format(matrix.asInstanceOf[StrideMatrix2D])
  }

  /**
   * Returns the index of the decimal point.
   */
  protected def indexOfDecimalPoint(s: String): Int = {
    var i = s.lastIndexOf('.')
    if (i < 0) i = s.lastIndexOf('e')
    if (i < 0) i = s.lastIndexOf('E')
    if (i < 0) i = s.length
    i
  }

  /**
   * Returns the number of characters before the decimal point.
   */
  override protected def lead(s: String): Int = {
    if (alignment == AbstractFormatter.DECIMAL) return indexOfDecimalPoint(s)
    super.lead(s)
  }

  /**
   * Returns a string <tt>s</tt> such that <tt>Object[] m = s</tt> is a legal
   * Java statement.
   *
   * @param matrix
   *            the matrix to format.
   */
  def toSourceCode(matrix: StrideMatrix1D): String = {
    val copy = this.clone().asInstanceOf[DoubleFormatter]
    copy.setPrintShape(printShape=false)
    copy.setColumnSeparator(", ")
    "{" + copy.toString(matrix) + "};"
  }

  /**
   * Returns a string <tt>s</tt> such that <tt>Object[] m = s</tt> is a legal
   * Java statement.
   *
   * @param matrix
   *            the matrix to format.
   */
  def toSourceCode(matrix: StrideMatrix2D): String = {
    val copy = this.clone().asInstanceOf[DoubleFormatter]
    val b3 = blanks(3)
    copy.setPrintShape(printShape=false)
    copy.setColumnSeparator(", ")
    copy.setRowSeparator("},\n" + b3 + "{")
    "{\n" + b3 + "{" + copy.toString(matrix) + "}\n};"
  }

  /**
   * Returns a string representation of the given matrix.
   *
   * @param matrix
   *            the matrix to convert.
   */
  def toString(matrix: StrideMatrix1D): String = {
    val easy = matrix.like2D(1, matrix.size.toInt)
    easy.viewRow(0).assign(matrix)
    toString(easy)
  }

  /**
   * Returns a string representation of the given matrix.
   *
   * @param matrix
   *            the matrix to convert.
   */
  def toString(matrix: StrideMatrix2D[_]): String = super.toString(matrix)

  /**
   * Returns a string representation of the given matrix.
   *
   * @param matrix
   *            the matrix to convert.
   */
  protected def toString(matrix: AbstractMatrix2D[_]): String = {
    this.toString(matrix.asInstanceOf[StrideMatrix2D[_]])
  }

  /**
   * Returns a string representation of the given matrix with axis as well as
   * rows and columns labeled. Pass <tt>null</tt> to one or more parameters to
   * indicate that the corresponding decoration element shall not appear in
   * the string converted matrix.
   *
   * @param matrix
   *            The matrix to format.
   * @param rowNames
   *            The headers of all rows (to be put to the left of the matrix).
   * @param columnNames
   *            The headers of all columns (to be put to above the matrix).
   * @param rowAxisName
   *            The label of the y-axis.
   * @param columnAxisName
   *            The label of the x-axis.
   * @param title
   *            The overall title of the matrix to be formatted.
   * @return the matrix converted to a string.
   */
  protected def toTitleString(matrix: StrideMatrix2D,
      rowNames: Array[String],
      columnNames: Array[String],
      rowAxisName: String,
      columnAxisName: String,
      title: String): String = {
    if (matrix.size == 0) return "Empty matrix"
    val s = format(matrix)
    align(s)
    new cern.colt.matrix.tobject.algo.ObjectFormatter()
      .toTitleString(cern.colt.matrix.tobject.ObjectFactory2D.dense.make(s), rowNames, columnNames, rowAxisName,
      columnAxisName, title)
  }

  /**
   * Same as <tt>toTitleString</tt> except that additionally statistical
   * aggregates (mean, median, sum, etc.) of rows and columns are printed.
   * Pass <tt>null</tt> to one or more parameters to indicate that the
   * corresponding decoration element shall not appear in the string converted
   * matrix.
   *
   * @param matrix
   *            The matrix to format.
   * @param rowNames
   *            The headers of all rows (to be put to the left of the matrix).
   * @param columnNames
   *            The headers of all columns (to be put to above the matrix).
   * @param rowAxisName
   *            The label of the y-axis.
   * @param columnAxisName
   *            The label of the x-axis.
   * @param title
   *            The overall title of the matrix to be formatted.
   * @param aggr
   *            the aggregation functions to be applied to columns and rows.
   * @return the matrix converted to a string.
   * @see hep.aida.tdouble.bin.DoubleBinFunction1D
   * @see hep.aida.tdouble.bin.DoubleBinFunctions1D
   */
  def toTitleString(matrix: StrideMatrix2D,
      rowNames: Array[String],
      columnNames: Array[String],
      rowAxisName: String,
      columnAxisName: String,
      title: String,
      aggr: Array[hep.aida.tdouble.bin.DoubleBinFunction1D]): String = {
    if (matrix.size == 0) return "Empty matrix"
    if (aggr == null || aggr.length == 0) return toTitleString(matrix, rowNames, columnNames, rowAxisName,
      columnAxisName, title)
    var rowStats = matrix.like(matrix.rows(), aggr.length)
    var colStats = matrix.like(aggr.length, matrix.columns())
    cern.colt.matrix.tdouble.algo.DoubleStatistic.aggregate(matrix, aggr, colStats)
    cern.colt.matrix.tdouble.algo.DoubleStatistic.aggregate(matrix.viewDice(), aggr, rowStats.viewDice())
    var tmp = matrix.like(matrix.rows() + aggr.length, matrix.columns())
    tmp.viewPart(0, 0, matrix.rows(), matrix.columns())
      .assign(matrix)
    tmp.viewPart(matrix.rows(), 0, aggr.length, matrix.columns())
      .assign(colStats)
    colStats = null
    var s1 = format(tmp)
    align(s1)
    tmp = null
    var s2 = format(rowStats)
    align(s2)
    rowStats = null
    val allStats = cern.colt.matrix.tobject.ObjectFactory2D.dense.make(matrix.rows() + aggr.length, matrix.columns() + aggr.length + 1)
    allStats.viewPart(0, 0, matrix.rows() + aggr.length, matrix.columns())
      .assign(s1)
    allStats.viewColumn(matrix.columns()).assign("|")
    allStats.viewPart(0, matrix.columns() + 1, matrix.rows(), aggr.length)
      .assign(s2)
    s1 = null
    s2 = null
    if (columnNames != null) {
      val list = new cern.colt.list.tobject.ObjectArrayList(columnNames)
      list.add("|")
      for (i <- 0 until aggr.length) list.add(aggr(i).name())
      columnNames = Array.ofDim[String](list.size)
      list.toArray(columnNames)
    }
    if (rowNames != null) {
      val list = new cern.colt.list.tobject.ObjectArrayList(rowNames)
      for (i <- 0 until aggr.length) list.add(aggr(i).name())
      rowNames = Array.ofDim[String](list.size)
      list.toArray(rowNames)
    }
    val s = new cern.colt.matrix.tobject.algo.ObjectFormatter()
      .toTitleString(allStats, rowNames, columnNames, rowAxisName, columnAxisName, title)
    var last = s.length + 1
    var secondLast = last
    val v = Math.max(0, if (rowAxisName == null) 0 else rowAxisName.length - matrix.rows() - aggr.length)
    for (k <- 0 until aggr.length + 1 + v) {
      secondLast = last
      last = s.lastIndexOf(rowSeparator, last - 1)
    }
    val buf = new StringBuffer(s)
    buf.insert(secondLast, rowSeparator + repeat('-', secondLast - last - 1))
    buf.toString
  }
}
