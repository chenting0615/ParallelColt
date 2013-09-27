package cern.colt.matrix.io

/**
 * Contains the size of a matrix stored in the <a
 * href="http://math.nist.gov/MatrixMarket">Matrix Market</a> exchange format
 */
class MatrixSize(var numRows: Int, var numColumns: Int, info: MatrixInfo) {

  /**
   * Number of entries stored
   */
  var numEntries: Int = _

  if (!info.isDense) throw new IllegalArgumentException("Matrix must be dense")

  if (info.isGeneral) numEntries = numRows * numColumns else if (info.isSymmetric || info.isHermitian) numEntries = (numRows * numColumns - numRows) / 2 + numRows else if (info.isSkewSymmetric) numEntries = (numRows * numColumns - numRows) / 2

  /**
   * Constructor for MatrixSize
   *
   * @param numRows
   *            Number of rows in the matrix
   * @param numColumns
   *            Number of columns in the matrix
   * @param numEntries
   *            Number of entries stored
   */
  def this(numRows: Int, numColumns: Int, numEntries: Int) {
    this()
    this.numRows = numRows
    this.numColumns = numColumns
    this.numEntries = numEntries
    val maxR = numRows
    val maxC = numColumns
    val max = maxR * maxC
    if (numEntries > max) throw new IllegalArgumentException("numEntries > numRows*numColumns")
  }

  /**
   * Returns <code>true</code> if the matrix is square, else
   * <code>false</code>
   */
  def isSquare: Boolean = numRows == numColumns
}
