package cern.jet.stat.quantile

import cern.jet.random.tdouble.DoubleUniform
import cern.jet.random.tdouble.engine.DRand
import hep.aida.tdouble.bin.DynamicDoubleBin1D
import hep.aida.tdouble.bin.QuantileDoubleBin1D
import java.text.DecimalFormat
import java.util.Date
//remove if not needed
import scala.collection.JavaConversions._

object CliTestQuantile1 {

  def main(argv: Array[String]) {
    var numExamples = 0
    try {
      numExamples = Integer.parseInt(argv(0))
    } catch {
      case e: Exception => {
        System.err.println("Unable to parse input line count argument")
        System.err.println(e.getMessage)
        System.exit(1)
      }
    }
    println("Got numExamples=" + numExamples)
    var N = 0
    try {
      N = if (argv(1) == "L") Long.MAX_VALUE else if (argv(1) == "I") Integer.MAX_VALUE else Long.parseLong(argv(1))
    } catch {
      case e: Exception => {
        System.err.println("Error parsing flag for N")
        System.err.println(e.getMessage)
        System.exit(1)
      }
    }
    println("Got N=" + N)
    val rand = new DRand(new Date())
    val qAccum = new QuantileDoubleBin1D(false, N, 1.e-4, 1.e-3, 200, rand, false, false, 2)
    val dbin = new DynamicDoubleBin1D()
    val dataRand = new DoubleUniform(new DRand(7757))
    var i = 1
    while (i <= numExamples) {
      val gauss = dataRand.nextDouble()
      qAccum.add(gauss)
      dbin.add(gauss)
      i += 1
    }
    val fmt = new DecimalFormat("0.00")
    println()
    val step = 10
    var i = 1
    while (i < 100) {
      val percent = (i) * 0.01
      val quantile = qAccum.quantile(percent)
      println(fmt.format(percent) + "  " + quantile + ",  " + dbin.quantile(percent) + 
        ",  " + 
        (dbin.quantile(percent) - quantile))
      i = i + step
    }
  }
}
