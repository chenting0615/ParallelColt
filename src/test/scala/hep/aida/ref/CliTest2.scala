package hep.aida.ref

import hep.aida.tdouble.DoubleIHistogram1D
import hep.aida.tdouble.DoubleIHistogram2D
import hep.aida.tdouble.DoubleIHistogram3D
import hep.aida.tdouble.ref.DoubleConverter
import hep.aida.tdouble.ref.DoubleHistogram1D
import hep.aida.tdouble.ref.DoubleHistogram2D
import hep.aida.tdouble.ref.DoubleHistogram3D
import hep.aida.tdouble.ref.DoubleVariableAxis
import java.io.FileWriter
import java.io.IOException
import java.io.PrintWriter
import java.util.Random
//remove if not needed
import scala.collection.JavaConversions._

object CliTest2 {

  def main2(argv: Array[String]) {
    val r = new Random()
    val h1 = new DoubleHistogram1D("AIDA 1D Histogram", 40, -3, 3)
    for (i <- 0 until 10000) h1.fill(r.nextGaussian())
    val h2 = new DoubleHistogram2D("AIDA 2D Histogram", 40, -3, 3, 40, -3, 3)
    for (i <- 0 until 10000) h2.fill(r.nextGaussian(), r.nextGaussian())
    writeAsXML(h1, "aida1.xml")
    writeAsXML(h2, "aida2.xml")
    writeAsXML(h2.projectionX(), "projectionX.xml")
    writeAsXML(h2.projectionY(), "projectionY.xml")
  }

  def main(argv: Array[String]) {
    val bounds = Array(-30, 0, 30, 1000)
    val r = new Random()
    val h1 = new DoubleHistogram1D("AIDA 1D Histogram", new DoubleVariableAxis(bounds))
    for (i <- 0 until 10000) h1.fill(r.nextGaussian())
    val h2 = new DoubleHistogram2D("AIDA 2D Histogram", new DoubleVariableAxis(bounds), new DoubleVariableAxis(bounds))
    for (i <- 0 until 10000) h2.fill(r.nextGaussian(), r.nextGaussian())
    val h3 = new DoubleHistogram3D("AIDA 3D Histogram", 10, -2, +2, 5, -2, +2, 3, -2, +2)
    for (i <- 0 until 10000) h3.fill(r.nextGaussian(), r.nextGaussian(), r.nextGaussian())
    writeAsXML(h1, "aida1.xml")
    writeAsXML(h2, "aida2.xml")
    writeAsXML(h3, "aida2.xml")
    writeAsXML(h2.projectionX(), "projectionX.xml")
    writeAsXML(h2.projectionY(), "projectionY.xml")
  }

  private def writeAsXML(h: DoubleIHistogram1D, filename: String) {
    try {
      val out = new PrintWriter(new FileWriter(filename))
      out.println(new DoubleConverter().toXML(h))
      out.close()
    } catch {
      case x: IOException => x.printStackTrace()
    }
  }

  private def writeAsXML(h: DoubleIHistogram2D, filename: String) {
    try {
      val out = new PrintWriter(new FileWriter(filename))
      out.println(new DoubleConverter().toXML(h))
      out.close()
    } catch {
      case x: IOException => x.printStackTrace()
    }
  }

  private def writeAsXML(h: DoubleIHistogram3D, filename: String) {
    println(new DoubleConverter() toString h)
  }
}
