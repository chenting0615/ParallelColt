package cern.jet.stat

//remove if not needed
import scala.collection.JavaConversions._

/**
 * An abstract set of buffers; internally used for computing approximate
 * quantiles.
 */
@SerialVersionUID(1L)
abstract class BufferSet extends cern.colt.PersistentObject
