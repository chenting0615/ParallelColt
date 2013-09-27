package hep.aida.tdouble.ref

import scala.reflect.{BeanProperty, BooleanBeanProperty}
//remove if not needed
import scala.collection.JavaConversions._

class DoubleHistogram1DContents(@BeanProperty var entries: Array[Int], 
    @BeanProperty var heights: Array[Double], 
    @BeanProperty var errors: Array[Double], 
    var nEntry: Int, 
    @BeanProperty var sumWeight: Double, 
    @BeanProperty var sumWeightSquared: Double, 
    @BeanProperty var mean: Double, 
    @BeanProperty var rms: Double) {

  def getNentry(): Int = nEntry
}
