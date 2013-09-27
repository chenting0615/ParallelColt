package hep.aida.ref

import hep.aida.tdouble.DoubleIHistogram
import hep.aida.tdouble.DoubleIHistogram1D
import hep.aida.tdouble.DoubleIHistogram2D
import hep.aida.tdouble.ref.DoubleHistogram1D
import hep.aida.tdouble.ref.DoubleHistogram2D
import java.io.FileWriter
import java.io.IOException
import java.io.PrintWriter
import java.util.Random
//remove if not needed
import scala.collection.JavaConversions._

object CliTest1 {

  def main(argv: Array[String]) {
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

  private def writeAsXML(h: DoubleIHistogram1D, filename: String) {
    try {
      val out = new PrintWriter(new FileWriter(filename))
      out.println("<?xml version=\"1.0\" encoding=\"ISO-8859-1\" ?>")
      out.println("<!DOCTYPE plotML SYSTEM \"plotML.dtd\">")
      out.println("<plotML>")
      out.println("<plot>")
      out.println("<dataArea>")
      out.println("<data1d>")
      out.println("<bins1d title=\"" + h.title() + "\">")
      for (i <- 0 until h.xAxis().bins()) {
        out.println(h.binEntries(i) + "," + h.binError(i))
      }
      out.println("</bins1d>")
      out.print("<binnedDataAxisAttributes type=\"double\" axis=\"x0\"")
      out.print(" min=\"" + h.xAxis().lowerEdge() + "\"")
      out.print(" max=\"" + h.xAxis().upperEdge() + "\"")
      out.print(" numberOfBins=\"" + h.xAxis().bins() + "\"")
      out.println("/>")
      out.println("<statistics>")
      out.println("<statistic name=\"Entries\" value=\"" + h.entries() + 
        "\"/>")
      out.println("<statistic name=\"Underflow\" value=\"" + h.binEntries(DoubleIHistogram.UNDERFLOW) + 
        "\"/>")
      out.println("<statistic name=\"Overflow\" value=\"" + h.binEntries(DoubleIHistogram.OVERFLOW) + 
        "\"/>")
      if (!Double.isNaN(h.mean())) out.println("<statistic name=\"Mean\" value=\"" + h.mean() + "\"/>")
      if (!Double.isNaN(h.rms())) out.println("<statistic name=\"RMS\" value=\"" + h.rms() + "\"/>")
      out.println("</statistics>")
      out.println("</data1d>")
      out.println("</dataArea>")
      out.println("</plot>")
      out.println("</plotML>")
      out.close()
    } catch {
      case x: IOException => x.printStackTrace()
    }
  }

  private def writeAsXML(h: DoubleIHistogram2D, filename: String) {
    try {
      val out = new PrintWriter(new FileWriter(filename))
      out.println("<?xml version=\"1.0\" encoding=\"ISO-8859-1\" ?>")
      out.println("<!DOCTYPE plotML SYSTEM \"plotML.dtd\">")
      out.println("<plotML>")
      out.println("<plot>")
      out.println("<dataArea>")
      out.println("<data2d type=\"xxx\">")
      out.println("<bins2d title=\"" + h.title() + "\" xSize=\"" + h.xAxis().bins() + 
        "\" ySize=\"" + 
        h.yAxis().bins() + 
        "\">")
      for (i <- 0 until h.xAxis().bins(); j <- 0 until h.yAxis().bins()) {
        out.println(h.binEntries(i, j) + "," + h.binError(i, j))
      }
      out.println("</bins2d>")
      out.print("<binnedDataAxisAttributes type=\"double\" axis=\"x0\"")
      out.print(" min=\"" + h.xAxis().lowerEdge() + "\"")
      out.print(" max=\"" + h.xAxis().upperEdge() + "\"")
      out.print(" numberOfBins=\"" + h.xAxis().bins() + "\"")
      out.println("/>")
      out.print("<binnedDataAxisAttributes type=\"double\" axis=\"y0\"")
      out.print(" min=\"" + h.yAxis().lowerEdge() + "\"")
      out.print(" max=\"" + h.yAxis().upperEdge() + "\"")
      out.print(" numberOfBins=\"" + h.yAxis().bins() + "\"")
      out.println("/>")
      out.println("</data2d>")
      out.println("</dataArea>")
      out.println("</plot>")
      out.println("</plotML>")
      out.close()
    } catch {
      case x: IOException => x.printStackTrace()
    }
  }
}
