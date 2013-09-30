package cern.colt.matrix

import AbstractFormatter._

object AbstractFormatter {

  /**
   * The alignment string aligning the cells of a column to the left.
   */
  val LEFT = "left"

  /**
   * The alignment string aligning the cells of a column to its center.
   */
  val CENTER = "center"

  /**
   * The alignment string aligning the cells of a column to the right.
   */
  val RIGHT = "right"

  /**
   * The alignment string aligning the cells of a column to the decimal point.
   */
  val DECIMAL = "decimal"

  /**
   * The default minimum number of characters a column may have; currently
   * <tt>1</tt>.
   */
  val DEFAULT_MIN_COLUMN_WIDTH = 1

  /**
   * The default string separating any two columns from another; currently
   * <tt>" "</tt>.
   */
  val DEFAULT_COLUMN_SEPARATOR = " "

  /**
   * The default string separating any two rows from another; currently
   * <tt>"\n"</tt>.
   */
  val DEFAULT_ROW_SEPARATOR = "\n"

  /**
   * The default string separating any two slices from another; currently
   * <tt>"\n\n"</tt>.
   */
  val DEFAULT_SLICE_SEPARATOR = "\n\n"

  private var blanksCache: Array[String] = _

  protected val factory = new FormatterFactory()

  setupBlanksCache()

  /**
   * Cache for faster string processing.
   */
  protected def setupBlanksCache() {
    val size = 40
    blanksCache = Array.ofDim[String](size)
    val buf = new StringBuffer(size)
    var i = size
    while (i >= 0) {buf.append(' '); i -= 1}
    val str = buf.toString
    i = size
    while (i >= 0) {
      blanksCache(i) = str.substring(0, i)
      i -= 1
    }
  }

  /**
   * Returns a short string representation describing the shape of the matrix.
   */
  def shape(matrix: Matrix1D[_]): String = matrix.size + " matrix"

  /**
   * Returns a short string representation describing the shape of the matrix.
   */
  def shape(matrix: Matrix2D[_]): String = {
    matrix.rows + " x " + matrix.columns + " matrix"
  }
}

/**
 * Abstract base class for flexible, well human readable matrix print
 * formatting. Value type independent. A single cell is formatted via a format
 * string. Columns can be aligned left, centered, right and by decimal point.
 * <p>
 * A column can be broader than specified by the parameter
 * <tt>minColumnWidth</tt> (because a cell may not fit into that width) but a
 * column is never smaller than <tt>minColumnWidth</tt>. Normally one does not
 * need to specify <tt>minColumnWidth</tt>. Cells in a row are separated by a
 * separator string, similar separators can be set for rows and slices. For more
 * info, see the concrete subclasses.
 *
 * @author wolfgang.hoschek@cern.ch
 * @version 1.0, 09/24/99
 */
@SerialVersionUID(1L)
abstract class AbstractFormatter protected () extends cern.colt.PersistentObject {

  /**
   * The default format string for formatting a single cell value; currently
   * <tt>"%G"</tt>.
   */
  protected var alignment: String = LEFT

  /**
   * The default format string for formatting a single cell value; currently
   * <tt>"%G"</tt>.
   */
  protected var format: String = "%G"

  /**
   * The default minimum number of characters a column may have; currently
   * <tt>1</tt>.
   */
  protected var minColumnWidth: Int = DEFAULT_MIN_COLUMN_WIDTH

  /**
   * The default string separating any two columns from another; currently
   * <tt>" "</tt>.
   */
  protected var columnSeparator: String = DEFAULT_COLUMN_SEPARATOR

  /**
   * The default string separating any two rows from another; currently
   * <tt>"\n"</tt>.
   */
  protected var rowSeparator: String = DEFAULT_ROW_SEPARATOR

  /**
   * The default string separating any two slices from another; currently
   * <tt>"\n\n"</tt>.
   */
  protected var sliceSeparator: String = DEFAULT_SLICE_SEPARATOR

  /**
   * Tells whether String representations are to be preceded with summary of
   * the shape; currently <tt>true</tt>.
   */
  protected var printShape: Boolean = true

  /**
   * Modifies the strings in a column of the string matrix to be aligned
   * (left,centered,right,decimal).
   */
  protected def align(strings: Array[Array[String]]) {
    val rows = strings.length
    var columns = 0
    if (rows > 0) columns = strings(0).length
    val maxColWidth = Array.ofDim[Int](columns)
    var maxColLead: Array[Int] = null
    val isDecimal = alignment == DECIMAL
    if (isDecimal) maxColLead = Array.ofDim[Int](columns)
    for (column <- 0 until columns) {
      var maxWidth = minColumnWidth
      var maxLead = Integer.MIN_VALUE
      for (row <- 0 until rows) {
        val s = strings(row)(column)
        maxWidth = Math.max(maxWidth, s.length)
        if (isDecimal) maxLead = Math.max(maxLead, lead(s))
      }
      maxColWidth(column) = maxWidth
      if (isDecimal) maxColLead(column) = maxLead
    }
    for (row <- 0 until rows) {
      alignRow(strings(row), maxColWidth, maxColLead)
    }
  }

  /**
   * Converts a row into a string.
   */
  protected def alignmentCode(alignment: String): Int = {
    if (alignment == LEFT) -1 else if (alignment == CENTER) 0 else if (alignment == RIGHT) 1 else if (alignment == DECIMAL) 2 else throw new IllegalArgumentException("unknown alignment: " + alignment)
  }

  /**
   * Modifies the strings the string matrix to be aligned
   * (left,centered,right,decimal).
   */
  protected def alignRow(row: Array[String], maxColWidth: Array[Int], maxColLead: Array[Int]) {
    val s = new StringBuffer()
    val columns = row.length
    for (column <- 0 until columns) {
      s.setLength(0)
      val c = row(column)
      if (alignment == RIGHT) {
        s.append(blanks(maxColWidth(column) - s.length))
        s.append(c)
      } else if (alignment == DECIMAL) {
        s.append(blanks(maxColLead(column) - lead(c)))
        s.append(c)
        s.append(blanks(maxColWidth(column) - s.length))
      } else if (alignment == CENTER) {
        s.append(blanks((maxColWidth(column) - c.length) / 2))
        s.append(c)
        s.append(blanks(maxColWidth(column) - s.length))
      } else if (alignment == LEFT) {
        s.append(c)
        s.append(blanks(maxColWidth(column) - s.length))
      } else throw new InternalError()
      row(column) = s.toString
    }
  }

  /**
   * Returns a String with <tt>length</tt> blanks.
   */
  protected def blanks(lengthP: Int): String = {
    var length = lengthP
    if (length < 0) length = 0
    if (length < blanksCache.length) return blanksCache(length)
    val buf = new StringBuffer(length)
    for (k <- 0 until length) {
      buf.append(' ')
    }
    buf.toString
  }

  /**
   * Converts a given cell to a String; no alignment considered.
   */
  protected def format[T](matrix: Matrix1D[T], index: Int, formatter: Formatter): String

  /**
   * Returns a string representations of all cells; no alignment considered.
   */
  protected def format[T](matrix: Matrix2D[T]): Array[Array[String]]

  /**
   * Returns a string representations of all cells; no alignment considered.
   */
  protected def formatRow[T](vector: Matrix1D[T]): Array[String] = {
    var formatter: Formatter = null
    formatter = factory.create(format)
    val s = vector.size.toInt
    val strings = Array.ofDim[String](s)
    for (i <- 0 until s) {
      strings(i) = format(vector, i, formatter)
    }
    strings
  }

  /**
   * Returns the number of characters or the number of characters before the
   * decimal point.
   */
  protected def lead(s: String): Int = s.length

  /**
   * Returns a String with the given character repeated <tt>length</tt> times.
   */
  protected def repeat(character: Char, lengthP: Int): String = {
    var length = lengthP
    if (character == ' ') return blanks(length)
    if (length < 0) length = 0
    val buf = new StringBuffer(length)
    for (k <- 0 until length) {
      buf.append(character)
    }
    buf.toString
  }

  /**
   * Sets the column alignment (left,center,right,decimal).
   *
   * @param alignment
   *            the new alignment to be used; must be one of
   *            <tt>{LEFT,CENTER,RIGHT,DECIMAL}</tt>.
   */
  def setAlignment(alignment: String) {
    this.alignment = alignment
  }

  /**
   * Sets the string separating any two columns from another.
   *
   * @param columnSeparator
   *            the new columnSeparator to be used.
   */
  def setColumnSeparator(columnSeparator: String) {
    this.columnSeparator = columnSeparator
  }

  /**
   * Sets the way a <i>single</i> cell value is to be formatted.
   *
   * @param format
   *            the new format to be used.
   */
  def setFormat(format: String) {
    this.format = format
  }

  /**
   * Sets the minimum number of characters a column may have.
   *
   * @param minColumnWidth
   *            the new minColumnWidth to be used.
   */
  def setMinColumnWidth(minColumnWidth: Int) {
    if (minColumnWidth < 0) throw new IllegalArgumentException()
    this.minColumnWidth = minColumnWidth
  }

  /**
   * Specifies whether a string representation of a matrix is to be preceded
   * with a summary of its shape.
   *
   * @param printShape
   *            <tt>true</tt> shape summary is printed, otherwise not printed.
   */
  def setPrintShape(printShape: Boolean) {
    this.printShape = printShape
  }

  /**
   * Sets the string separating any two rows from another.
   *
   * @param rowSeparator
   *            the new rowSeparator to be used.
   */
  def setRowSeparator(rowSeparator: String) {
    this.rowSeparator = rowSeparator
  }

  /**
   * Sets the string separating any two slices from another.
   *
   * @param sliceSeparator
   *            the new sliceSeparator to be used.
   */
  def setSliceSeparator(sliceSeparator: String) {
    this.sliceSeparator = sliceSeparator
  }

  /**
   * Returns a single string representation of the given string matrix.
   *
   * @param strings
   *            the matrix to be converted to a single string.
   */
  protected def toString(strings: Array[Array[String]]): String = {
    val rows = strings.length
    val columns = if (strings.length <= 0) 0 else strings(0).length
    val total = new StringBuffer()
    val s = new StringBuffer()
    for (row <- 0 until rows) {
      s.setLength(0)
      for (column <- 0 until columns) {
        s.append(strings(row)(column))
        if (column < columns - 1) s.append(columnSeparator)
      }
      total.append(s)
      if (row < rows - 1) total.append(rowSeparator)
    }
    total.toString
  }

  /**
   * Returns a string representation of the given matrix.
   *
   * @param matrix
   *            the matrix to convert.
   */
  protected def toString[T](matrix: Matrix2D[T]): String = {
    val strings = this.format(matrix)
    align(strings)
    val total = new StringBuffer(strings.toString)
    if (printShape) total.insert(0, shape(matrix) + "\n")
    total.toString
  }
}
