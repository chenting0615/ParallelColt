package hep.aida.tdouble.ref

//remove if not needed
import scala.collection.JavaConversions._

/**
 * Base class for Histogram1D and Histogram2D.
 *
 * @author Wolfgang Hoschek, Tony Johnson, and others.
 * @version 1.0, 23/03/2000
 */
@SerialVersionUID(1L)
abstract class DoubleHistogram(var title: String) extends hep.aida.tdouble.DoubleIHistogram
