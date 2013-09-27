package cern.colt

//remove if not needed
import scala.collection.JavaConversions._

object Version {

  /**
   * Returns all version information as string.
   */
  def asString(): String = {
    if (getPackage == null) return "piotr.wendykier@gmail.com"
    var vendor = getPackage.getImplementationVendor
    if (vendor == null) vendor = "piotr.wendykier@gmail.com"
    "Version " + getMajorVersion + "." + getMinorVersion + 
      "." + 
      getMicroVersion + 
      "." + 
      getBuildVersion + 
      " (" + 
      getBuildTime + 
      ")" + 
      "\nPlease report problems to " + 
      vendor
  }

  /**
   * Returns the time this release was build; for example "Tue Apr 11 11:50:39
   * CEST 2000".
   */
  def getBuildTime(): String = {
    if (getPackage == null) return "unknown"
    val s = getPackage.getImplementationVersion
    if (s == null) return "unknown"
    val k = s.indexOf('(')
    s.substring(k + 1, s.length - 1)
  }

  /**
   * Returns the build version of this release.
   */
  def getBuildVersion(): Int = numbers()(3)

  /**
   * Returns the major version of this release.
   */
  def getMajorVersion(): Int = numbers()(0)

  /**
   * Returns the micro version of this release.
   */
  def getMicroVersion(): Int = numbers()(2)

  /**
   * Returns the minor version of this release.
   */
  def getMinorVersion(): Int = numbers()(1)

  /**
   *
   */
  private def getPackage(): Package = Package.getPackage("cern.colt")

  /**
   * Prints <tt>asString</tt> on <tt>System.out</tt>.
   *
   * @param args
   *            ignored.
   */
  def main(args: Array[String]) {
    println(asString())
  }

  /**
   * Returns the major version of this release; for example version 1.2.3
   * returns 1.
   */
  private def numbers(): Array[Int] = {
    val w = 4
    val numbers = Array(0, 6, 0, 0)
    numbers
  }
}
