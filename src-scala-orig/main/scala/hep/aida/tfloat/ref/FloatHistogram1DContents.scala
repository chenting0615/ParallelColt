package hep.aida.tfloat.ref

import scala.reflect.{BeanProperty, BooleanBeanProperty}
//remove if not needed
import scala.collection.JavaConversions._

class FloatHistogram1DContents(@BeanProperty var entries: Array[Int], 
    @BeanProperty var heights: Array[Float], 
    @BeanProperty var errors: Array[Float], 
    var nEntry: Int, 
    @BeanProperty var sumWeight: Float, 
    @BeanProperty var sumWeightSquared: Float, 
    @BeanProperty var mean: Float, 
    @BeanProperty var rms: Float) {

  def getNentry(): Int = nEntry
}
