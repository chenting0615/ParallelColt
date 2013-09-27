package cern.colt.matrix.io

import java.io.BufferedReader
import java.io.EOFException
import java.io.IOException
import java.io.Reader
import java.io.StreamTokenizer
import java.util

/**
 * Reads matrices and vectors
 * @param sz
 *            Input buffer size
 */
class MatrixVectorReader(in: Reader, sz: Int = 0) extends BufferedReader(in, sz) {

  /**
   * Reads the entries of the matrix or vector
   */
  private var st: StreamTokenizer = _

  setup()

  /**
   * Sets up the stream tokenizer
   */
  private def setup() {
    st = new StreamTokenizer(this)
    st.resetSyntax()
    st.eolIsSignificant(false)
    st.lowerCaseMode(true)
    st.wordChars('0', '9')
    st.wordChars('-', '.')
    st.wordChars(' ', 'ÿ')
    st.commentChar('%')
    st.whitespaceChars(' ', ' ')
    st.whitespaceChars('	', '')
  }

  /**
   * Shifts the indexes. Useful for converting between 0- and 1-based
   * indicing.
   *
   * @param num
   *            Added to every index
   * @param indexes
   *            indexes to shift
   */
  def add(num: Int, indexes: Array[Int]) {
    for (i <- 0 until indexes.length) indexes(i) += num
  }

  /**
   * Reads a line, and trims it of surrounding whitespace
   *
   * @throws IOException
   *             If either I/O errors occur, or there was nothing to read
   */
  private def readTrimmedLine(): String = {
    val line = readLine()
    if (line != null) line.trim() else throw new EOFException()
  }

  /**
   * Reads the matrix info for the Matrix Market exchange format. The line
   * must consist of exactly 5 space-separated entries, the first being
   * "%%MatrixMarket"
   */
  def readMatrixInfo(): MatrixInfo = {
    val component = readTrimmedLine().split(" +")
    if (component.length != 5) throw new IOException("Current line unparsable. It must consist of 5 tokens")
    if (!component(0).equalsIgnoreCase("%%MatrixMarket")) throw new IOException("Not in Matrix Market exchange format")
    if (!component(1).equalsIgnoreCase("matrix")) throw new IOException("Expected \"matrix\", got " + component(1))
    var sparse = false
    if (component(2).equalsIgnoreCase("coordinate")) sparse = true else if (component(2).equalsIgnoreCase("array")) sparse = false else throw new IOException("Unknown layout " + component(2))
    var field: MatrixField.MatrixField = null
    if (component(3).equalsIgnoreCase("real")) field = MatrixField.Real else if (component(3).equalsIgnoreCase("integer")) field = MatrixField.Integer else if (component(3).equalsIgnoreCase("complex")) field = MatrixField.Complex else if (component(3).equalsIgnoreCase("pattern")) field = MatrixField.Pattern else throw new IOException("Unknown field specification " + component(3))
    var symmetry: MatrixSymmetry.MatrixSymmetry = null
    if (component(4).equalsIgnoreCase("general")) symmetry = MatrixSymmetry.General else if (component(4).equalsIgnoreCase("symmetric")) symmetry = MatrixSymmetry.Symmetric else if (component(4).equalsIgnoreCase("skew-symmetric")) symmetry = MatrixSymmetry.SkewSymmetric else if (component(4).equalsIgnoreCase("Hermitian")) symmetry = MatrixSymmetry.Hermitian else throw new IOException("Unknown symmetry specification " + component(4))
    new MatrixInfo(sparse, field, symmetry)
  }

  /**
   * Reads the vector info for the Matrix Market exchange format. The line
   * must consist of exactly 4 space-separated entries, the first being
   * "%%MatrixMarket"
   */
  def readVectorInfo(): VectorInfo = {
    val component = readTrimmedLine().split(" +")
    if (component.length != 4) throw new IOException("Current line unparsable. It must consist of 4 tokens")
    if (!component(0).equalsIgnoreCase("%%MatrixMarket")) throw new IOException("Not in Matrix Market exchange format")
    if (!component(1).equalsIgnoreCase("vector")) throw new IOException("Expected \"vector\", got " + component(1))
    var sparse = false
    if (component(2).equalsIgnoreCase("coordinate")) sparse = true else if (component(2).equalsIgnoreCase("array")) sparse = false else throw new IOException("Unknown layout " + component(2))
    var field: VectorField.VectorField = null
    if (component(3).equalsIgnoreCase("real")) field = VectorField.Real else if (component(3).equalsIgnoreCase("integer")) field = VectorField.Integer else if (component(3).equalsIgnoreCase("complex")) field = VectorField.Complex else if (component(3).equalsIgnoreCase("pattern")) field = VectorField.Pattern else throw new IOException("Unknown field specification " + component(3))
    new VectorInfo(sparse, field)
  }

  /**
   * Checks if a Matrix Market header is present ("%%MatrixMarket")
   *
   * @return True if a header was found, else false
   * @throws IOException blah
   */
  def hasInfo: Boolean = {
    mark(1024)
    val component = readTrimmedLine().split(" +")
    reset()
    component(0).equalsIgnoreCase("%%MatrixMarket")
  }

  /**
   * Reads all the comments (lines starting with '%'). Positions the reader at
   * the first non-comment line. Can only be called after reading the matrix
   * or vector info. The comments read does not include '%' or the newline
   */
  def readComments(): Array[String] = {
    val list = new util.LinkedList[String]()
    while (true) {
      mark(1024)
      val line = readTrimmedLine()
      if (line.length > 0) if (line.charAt(0) != '%') {
        reset()
        //break
      } else list.add(line.substring(1))
    }
    list.toArray(Array.ofDim[String](list.size))
  }

  /**
   * Reads in the size of a matrix. Skips initial comments
   */
  def readMatrixSize(info: MatrixInfo): MatrixSize = {
    val numRows = getInt
    val numColumns = getInt
    if (info.isDense) new MatrixSize(numRows, numColumns, info) else {
      val numEntries = getInt
      new MatrixSize(numRows, numColumns, numEntries)
    }
  }

  /**
   * Reads in the size of an array matrix. Skips initial comments
   */
  def readArraySize(): MatrixSize = {
    val numRows = getInt
    val numColumns = getInt
    new MatrixSize(numRows, numColumns, numRows * numColumns)
  }

  /**
   * Reads in the size of a coordinate matrix. Skips initial comments
   */
  def readCoordinateSize(): MatrixSize = {
    val numRows = getInt
    val numColumns = getInt
    val numEntries = getInt
    new MatrixSize(numRows, numColumns, numEntries)
  }

  /**
   * Reads in the size of a vector. Skips initial comments
   */
  def readVectorSize(info: VectorInfo): VectorSize = {
    val size = getInt
    if (info.isDense) new VectorSize(size) else {
      val numEntries = getInt
      new VectorSize(size, numEntries)
    }
  }

  /**
   * Reads in the size of a dense vector. Skips initial comments
   */
  def readVectorArraySize(): VectorSize = {
    val size = getInt
    new VectorSize(size)
  }

  /**
   * Reads in the size of a coordinate vector. Skips initial comments
   */
  def readVectorCoordinateSize(): VectorSize = {
    val size = getInt
    val numEntries = getInt
    new VectorSize(size, numEntries)
  }

  /**
   * Reads the array data
   */
  def readArray(data: Array[Double]) {
    val size = data.length
    for (i <- 0 until size) data(i) = getDouble
  }

  /**
   * Reads the array data
   */
  def readArray(data: Array[Float]) {
    val size = data.length
    for (i <- 0 until size) data(i) = getFloat
  }

  /**
   * Reads the array data
   */
  def readArray(data: Array[Int]) {
    val size = data.length
    for (i <- 0 until size) data(i) = getInt
  }

  /**
   * Reads the array data
   */
  def readArray(data: Array[Long]) {
    val size = data.length
    for (i <- 0 until size) data(i) = getLong
  }

  /**
   * Reads the array data. The first array will contain real entries, while
   * the second contain imaginary entries
   */
  def readArray(dataR: Array[Double], dataI: Array[Double]) {
    val size = dataR.length
    if (size != dataI.length) throw new IllegalArgumentException("All arrays must be of the same size")
    for (i <- 0 until size) {
      dataR(i) = getDouble
      dataI(i) = getDouble
    }
  }

  /**
   * Reads the array data. The first array will contain real entries, while
   * the second contain imaginary entries
   */
  def readArray(dataR: Array[Float], dataI: Array[Float]) {
    val size = dataR.length
    if (size != dataI.length) throw new IllegalArgumentException("All arrays must be of the same size")
    for (i <- 0 until size) {
      dataR(i) = getFloat
      dataI(i) = getFloat
    }
  }

  /**
   * Reads a coordinate vector
   */
  def readCoordinate(index: Array[Int], data: Array[Double]) {
    val size = index.length
    if (size != data.length) throw new IllegalArgumentException("All arrays must be of the same size")
    for (i <- 0 until size) {
      index(i) = getInt - 1
      data(i) = getDouble
    }
  }

  /**
   * Reads a coordinate vector
   */
  def readCoordinate(index: Array[Int], data: Array[Float]) {
    val size = index.length
    if (size != data.length) throw new IllegalArgumentException("All arrays must be of the same size")
    for (i <- 0 until size) {
      index(i) = getInt - 1
      data(i) = getFloat
    }
  }

  /**
   * Reads a coordinate vector
   */
  def readCoordinate(index: Array[Int], data: Array[Int]) {
    val size = index.length
    if (size != data.length) throw new IllegalArgumentException("All arrays must be of the same size")
    for (i <- 0 until size) {
      index(i) = getInt - 1
      data(i) = getInt
    }
  }

  /**
   * Reads a coordinate vector
   */
  def readCoordinate(index: Array[Int], data: Array[Long]) {
    val size = index.length
    if (size != data.length) throw new IllegalArgumentException("All arrays must be of the same size")
    for (i <- 0 until size) {
      index(i) = getInt - 1
      data(i) = getLong
    }
  }

  /**
   * Reads a coordinate vector. First data array contains real entries, and
   * the second contains imaginary entries
   */
  def readCoordinate(index: Array[Int], dataR: Array[Float], dataI: Array[Float]) {
    val size = index.length
    if (size != dataR.length || size != dataI.length) throw new IllegalArgumentException("All arrays must be of the same size")
    for (i <- 0 until size) {
      index(i) = getInt - 1
      dataR(i) = getFloat
      dataI(i) = getFloat
    }
  }

  /**
   * Reads a coordinate vector. First data array contains real entries, and
   * the second contains imaginary entries
   */
  def readCoordinate(index: Array[Int], dataR: Array[Double], dataI: Array[Double]) {
    val size = index.length
    if (size != dataR.length || size != dataI.length) throw new IllegalArgumentException("All arrays must be of the same size")
    for (i <- 0 until size) {
      index(i) = getInt - 1
      dataR(i) = getDouble
      dataI(i) = getDouble
    }
  }

  /**
   * Reads a pattern vector
   */
  def readPattern(index: Array[Int]) {
    val size = index.length
    for (i <- 0 until size) index(i) = getInt - 1
  }

  /**
   * Reads a coordinate matrix
   */
  def readCoordinate(row: Array[Int], column: Array[Int], data: Array[Double]) {
    val size = row.length
    if (size != column.length || size != data.length) throw new IllegalArgumentException("All arrays must be of the same size")
    for (i <- 0 until size) {
      row(i) = getInt - 1
      column(i) = getInt - 1
      data(i) = getDouble
    }
  }

  /**
   * Reads a coordinate matrix
   */
  def readCoordinate(row: Array[Int], column: Array[Int], data: Array[Float]) {
    val size = row.length
    if (size != column.length || size != data.length) throw new IllegalArgumentException("All arrays must be of the same size")
    for (i <- 0 until size) {
      row(i) = getInt - 1
      column(i) = getInt - 1
      data(i) = getFloat
    }
  }

  /**
   * Reads a coordinate matrix
   */
  def readCoordinate(row: Array[Int], column: Array[Int], data: Array[Int]) {
    val size = row.length
    if (size != column.length || size != data.length) throw new IllegalArgumentException("All arrays must be of the same size")
    for (i <- 0 until size) {
      row(i) = getInt - 1
      column(i) = getInt - 1
      data(i) = getInt
    }
  }

  /**
   * Reads a coordinate matrix
   */
  def readCoordinate(row: Array[Int], column: Array[Int], data: Array[Long]) {
    val size = row.length
    if (size != column.length || size != data.length) throw new IllegalArgumentException("All arrays must be of the same size")
    for (i <- 0 until size) {
      row(i) = getInt - 1
      column(i) = getInt - 1
      data(i) = getLong
    }
  }

  /**
   * Reads a pattern matrix
   */
  def readPattern(row: Array[Int], column: Array[Int]) {
    val size = row.length
    if (size != column.length) throw new IllegalArgumentException("All arrays must be of the same size")
    for (i <- 0 until size) {
      row(i) = getInt - 1
      column(i) = getInt - 1
    }
  }

  /**
   * Reads a coordinate matrix. First data array contains real entries, and
   * the second contains imaginary entries
   */
  def readCoordinate(row: Array[Int],
      column: Array[Int],
      dataR: Array[Double],
      dataI: Array[Double]) {
    val size = row.length
    if (size != column.length || size != dataR.length || size != dataI.length) throw new IllegalArgumentException("All arrays must be of the same size")
    for (i <- 0 until size) {
      row(i) = getInt - 1
      column(i) = getInt - 1
      dataR(i) = getDouble
      dataI(i) = getDouble
    }
  }

  /**
   * Reads a coordinate matrix. First data array contains real entries, and
   * the second contains imaginary entries
   */
  def readCoordinate(row: Array[Int],
      column: Array[Int],
      dataR: Array[Float],
      dataI: Array[Float]) {
    val size = row.length
    if (size != column.length || size != dataR.length || size != dataI.length) throw new IllegalArgumentException("All arrays must be of the same size")
    for (i <- 0 until size) {
      row(i) = getInt - 1
      column(i) = getInt - 1
      dataR(i) = getFloat
      dataI(i) = getFloat
    }
  }

  /**
   * Reads an integer
   */
  private def getInt: Int = {
    st.nextToken()
    if (st.ttype == StreamTokenizer.TT_WORD) Integer.parseInt(st.sval) else if (st.ttype == StreamTokenizer.TT_EOF) throw new EOFException("End-of-File encountered during parsing") else throw new IOException("Unknown token found during parsing")
  }

  /**
   * Reads a long
   */
  private def getLong: Long = {
    st.nextToken()
    if (st.ttype == StreamTokenizer.TT_WORD) st.sval.toLong else if (st.ttype == StreamTokenizer.TT_EOF) throw new EOFException("End-of-File encountered during parsing") else throw new IOException("Unknown token found during parsing")
  }

  /**
   * Reads a double
   */
  private def getDouble: Double = {
    st.nextToken()
    if (st.ttype == StreamTokenizer.TT_WORD)st.sval.toDouble else if (st.ttype == StreamTokenizer.TT_EOF) throw new EOFException("End-of-File encountered during parsing") else throw new IOException("Unknown token found during parsing")
  }

  /**
   * Reads a float
   */
  private def getFloat: Float = {
    st.nextToken()
    if (st.ttype == StreamTokenizer.TT_WORD) st.sval.toFloat else if (st.ttype == StreamTokenizer.TT_EOF) throw new EOFException("End-of-File encountered during parsing") else throw new IOException("Unknown token found during parsing")
  }
}
