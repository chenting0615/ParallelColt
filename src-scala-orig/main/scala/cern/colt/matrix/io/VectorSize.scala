package cern.colt.matrix.io

/**
 * Contains the size of a vector stored in a variant of the <a
 * href="http://math.nist.gov/MatrixMarket">Matrix Market</a> exchange format
 * @param size
 *            Size of the matrix
 * @param numEntries
 *            Number of entries stored
 */
class VectorSize(var size: Int, var numEntries: Int) {

  if (size < 0 || numEntries < 0) throw new IllegalArgumentException("size < 0 || numEntries < 0")
  if (numEntries > size) throw new IllegalArgumentException("numEntries > size")

  if (size < 0) throw new IllegalArgumentException("size < 0")

  /**
   * Constructor for VectorSize
   * @param numEntries
   *            Number of entries stored
   */
  def this(numEntries: Int) {
    this(0, numEntries)
  }
}
