package cern.colt

/**
 * This empty class is the common root for all persistent capable classes. If
 * this class inherits from <tt>java.lang.Object</tt> then all subclasses are
 * serializable with the standard Java serialization mechanism. If this class
 * inherits from <tt>com.objy.db.app.ooObj</tt> then all subclasses are
 * <i>additionally</i> serializable with the Objectivity ODBMS persistance
 * mechanism. Thus, by modifying the inheritance of this class the entire tree
 * of subclasses can be switched to Objectivity compatibility (and back) with
 * minimum effort.
 */
@SerialVersionUID(1020L)
trait PersistentObject extends AnyRef with java.io.Serializable with Cloneable {

  /**
   * Returns a copy of the receiver. This default implementation does not
   * nothing except making the otherwise <tt>protected</tt> clone method
   * <tt>public</tt>.
   *
   * @return a copy of the receiver.
   */
  override def clone(): AnyRef = super.clone()
}
