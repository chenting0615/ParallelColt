package cern.colt.matrix.tbit

import java.awt.Rectangle
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Fixed sized (non resizable) n*m bit matrix. A bit matrix has a number of
 * columns and rows, which are assigned upon instance construction - The
 * matrix's size is then <tt>columns()*rows()</tt>. Bits are accessed via
 * <tt>(column,row)</tt> coordinates.
 * <p>
 * Individual bits can be examined, set, or cleared. Rectangular parts (boxes)
 * can quickly be extracted, copied and replaced. Quick iteration over boxes is
 * provided by optimized internal iterators (<tt>forEach()</tt> methods). One
 * <code>BitMatrix</code> may be used to modify the contents of another
 * <code>BitMatrix</code> through logical AND, OR, XOR and other similar
 * operations.
 * <p>
 * Legal coordinates range from <tt>[0,0]</tt> to
 * <tt>[columns()-1,rows()-1]</tt>. Any attempt to access a bit at a coordinate
 * <tt>column&lt;0 || column&gt;=columns() || row&lt;0 || row&gt;=rows()</tt>
 * will throw an <tt>IndexOutOfBoundsException</tt>. Operations involving two
 * bit matrices (like AND, OR, XOR, etc.) will throw an
 * <tt>IllegalArgumentException</tt> if both bit matrices do not have the same
 * number of columns and rows.
 * <p>
 * If you need extremely quick access to individual bits: Although getting and
 * setting individual bits with methods <tt>get(...)</tt> and <tt>put(...)</tt>
 * is quick, it is even quicker (<b>but not safe</b>) to use
 * <tt>getQuick(...)</tt> and <tt>putQuick(...)</tt>.
 * <p>
 * <b>Note</b> that this implementation is not synchronized.
 *
 * @author wolfgang.hoschek@cern.ch
 * @version 1.0, 09/24/99
 * @see BitVector
 * @see QuickBitVector
 * @see java.util.BitSet
 */
@SerialVersionUID(1L)
class BitMatrix(columns: Int, rows: Int) extends cern.colt.PersistentObject {

  protected var columns: Int = _

  protected var rows: Int = _

  protected var bits: Long = _

  elements(QuickBitVector.makeBitVector(columns * rows, 1), columns, rows)

  /**
   * Performs a logical <b>AND</b> of the receiver with another bit matrix.
   * The receiver is modified so that a bit in it has the value
   * <code>true</code> if and only if it already had the value
   * <code>true</code> and the corresponding bit in the other bit matrix
   * argument has the value <code>true</code>.
   *
   * @param other
   *            a bit matrix.
   * @throws IllegalArgumentException
   *             if
   *             <tt>columns() != other.columns() || rows() != other.rows()</tt>
   *             .
   */
  def and(other: BitMatrix) {
    checkDimensionCompatibility(other)
    toBitVector().and(other.toBitVector())
  }

  /**
   * Clears all of the bits in receiver whose corresponding bit is set in the
   * other bit matrix. In other words, determines the difference (A\B) between
   * two bit matrices.
   *
   * @param other
   *            a bit matrix with which to mask the receiver.
   * @throws IllegalArgumentException
   *             if
   *             <tt>columns() != other.columns() || rows() != other.rows()</tt>
   *             .
   */
  def andNot(other: BitMatrix) {
    checkDimensionCompatibility(other)
    toBitVector().andNot(other.toBitVector())
  }

  /**
   * Returns the number of bits currently in the <tt>true</tt> state.
   * Optimized for speed. Particularly quick if the receiver is either sparse
   * or dense.
   */
  def cardinality(): Int = toBitVector().cardinality()

  /**
   * Sanity check for operations requiring matrices with the same number of
   * columns and rows.
   */
  protected def checkDimensionCompatibility(other: BitMatrix) {
    if (columns != other.columns() || rows != other.rows()) throw new IllegalArgumentException("Incompatible dimensions: (columns,rows)=(" + columns + 
      "," + 
      rows + 
      "), (other.columns,other.rows)=(" + 
      other.columns() + 
      "," + 
      other.rows() + 
      ")")
  }

  /**
   * Clears all bits of the receiver.
   */
  def clear() {
    toBitVector().clear()
  }

  /**
   * Cloning this <code>BitMatrix</code> produces a new <code>BitMatrix</code>
   * that is equal to it. The clone of the bit matrix is another bit matrix
   * that has exactly the same bits set to <code>true</code> as this bit
   * matrix and the same number of columns and rows.
   *
   * @return a clone of this bit matrix.
   */
  def clone(): AnyRef = {
    val clone = super.clone().asInstanceOf[BitMatrix]
    if (this.bits != null) clone.bits = this.bits.clone()
    clone
  }

  /**
   * Returns the number of columns of the receiver.
   */
  def columns(): Int = columns

  /**
   * Checks whether the receiver contains the given box.
   */
  protected def containsBox(column: Int, 
      row: Int, 
      width: Int, 
      height: Int) {
    if (column < 0 || column + width > columns || row < 0 || row + height > rows) throw new IndexOutOfBoundsException("column:" + column + ", row:" + row + " ,width:" + width + 
      ", height:" + 
      height)
  }

  /**
   * Returns a shallow clone of the receiver; calls <code>clone()</code> and
   * casts the result.
   *
   * @return a shallow clone of the receiver.
   */
  def copy(): BitMatrix = clone().asInstanceOf[BitMatrix]

  protected def elements(): Array[Long] = bits

  /**
   * You normally need not use this method. Use this method only if
   * performance is critical. Sets the bit matrix's backing bits, columns and
   * rows. <b>WARNING:</b> For efficiency reasons and to keep memory usage
   * low, <b>the array is not copied</b>. So if subsequently you modify the
   * specified array directly via the [] operator, be sure you know what
   * you're doing.
   *
   * @throws IllegalArgumentException
   *             if
   *             <tt>columns &lt; 0 || rows &lt; 0 || columns*rows &gt; bits.length*64</tt>
   */
  protected def elements(bits: Array[Long], columns: Int, rows: Int) {
    if (columns < 0 || columns < 0 || 
      columns * rows > bits.length * QuickBitVector.BITS_PER_UNIT) throw new IllegalArgumentException()
    this.bits = bits
    this.columns = columns
    this.rows = rows
  }

  /**
   * Compares this object against the specified object. The result is
   * <code>true</code> if and only if the argument is not <code>null</code>
   * and is a <code>BitMatrix</code> object that has the same number of
   * columns and rows as the receiver and that has exactly the same bits set
   * to <code>true</code> as the receiver.
   *
   * @param obj
   *            the object to compare with.
   * @return <code>true</code> if the objects are the same; <code>false</code>
   *         otherwise.
   */
  override def equals(obj: Any): Boolean = {
    if (obj == null || !(obj.isInstanceOf[BitMatrix])) return false
    if (this == obj) return true
    val other = obj.asInstanceOf[BitMatrix]
    if (columns != other.columns() || rows != other.rows()) return false
    toBitVector() == other.toBitVector()
  }

  /**
   * Applies a procedure to each coordinate that holds a bit in the given
   * state. Iterates rowwise downwards from [columns()-1,rows()-1] to [0,0].
   * Useful, for example, if you want to copy bits into an image or somewhere
   * else. Optimized for speed. Particularly quick if one of the following
   * conditions holds
   * <ul>
   * <li><tt>state==true</tt> and the receiver is sparse (
   * <tt>cardinality()</tt> is small compared to <tt>size()</tt>).
   * <li><tt>state==false</tt> and the receiver is dense (
   * <tt>cardinality()</tt> is large compared to <tt>size()</tt>).
   * </ul>
   *
   * @param state
   *            element to search for.
   * @param procedure
   *            a procedure object taking as first argument the current column
   *            and as second argument the current row. Stops iteration if the
   *            procedure returns <tt>false</tt>, otherwise continues.
   * @return <tt>false</tt> if the procedure stopped before all elements where
   *         iterated over, <tt>true</tt> otherwise.
   */
  def forEachCoordinateInState(state: Boolean, procedure: cern.colt.function.tint.IntIntProcedure): Boolean = {
    if (size == 0) return true
    val vector = new BitVector(bits, size)
    val theBits = bits
    var column = columns - 1
    val row = rows - 1
    var `val` = theBits(bits.length - 1)
    var j = vector.numberOfBitsInPartialUnit()
    while (j >= 0) {
      val mask = `val` & (1L << j)
      if ((state && (mask != 0L)) || ((!state) && (mask == 0L))) {
        if (!procedure.apply(column, row)) return false
      }
      if (column < 0) {
        column = columns - 1
        row
      }
    }
    val bitsPerUnit = QuickBitVector.BITS_PER_UNIT
    var comparator: Long = 0l
    comparator = if (state) 0L else ~0L
    var i = vector.numberOfFullUnits()
    while (i >= 0) {
      `val` = theBits(i)
      if (`val` != comparator) {
        if (state) {
          var j = bitsPerUnit
          while (j >= 0) {
            if (((`val` & (1L << j))) != 0L) {
              if (!procedure.apply(column, row)) return false
            }
            if (column < 0) {
              column = columns - 1
              row
            }
          }
        } else {
          var j = bitsPerUnit
          while (j >= 0) {
            if (((`val` & (1L << j))) == 0L) {
              if (!procedure.apply(column, row)) return false
            }
            if (column < 0) {
              column = columns - 1
              row
            }
          }
        }
      } else {
        column -= bitsPerUnit
        if (column < 0) {
          column += bitsPerUnit
          var j = bitsPerUnit
          while (j >= 0) {
            if (column < 0) {
              column = columns - 1
              row
            }
          }
        }
      }
    }
    true
  }

  /**
   * Returns from the receiver the value of the bit at the specified
   * coordinate. The value is <tt>true</tt> if this bit is currently set;
   * otherwise, returns <tt>false</tt>.
   *
   * @param column
   *            the index of the column-coordinate.
   * @param row
   *            the index of the row-coordinate.
   * @return the value of the bit at the specified coordinate.
   * @throws IndexOutOfBoundsException
   *             if
   *             <tt>column&lt;0 || column&gt;=columns() || row&lt;0 || row&gt;=rows()</tt>
   */
  def get(column: Int, row: Int): Boolean = {
    if (column < 0 || column >= columns || row < 0 || row >= rows) throw new IndexOutOfBoundsException("column:" + column + ", row:" + row)
    QuickBitVector.get(bits, row * columns + column)
  }

  /**
   * Returns from the receiver the value of the bit at the specified
   * coordinate; <b>WARNING:</b> Does not check preconditions. The value is
   * <tt>true</tt> if this bit is currently set; otherwise, returns
   * <tt>false</tt>.
   *
   * <p>
   * Provided with invalid parameters this method may return invalid values
   * without throwing any exception. <b>You should only use this method when
   * you are absolutely sure that the coordinate is within bounds.</b>
   * Precondition (unchecked):
   * <tt>column&gt;=0 && column&lt;columns() && row&gt;=0 && row&lt;rows()</tt>.
   *
   * @param column
   *            the index of the column-coordinate.
   * @param row
   *            the index of the row-coordinate.
   * @return the value of the bit at the specified coordinate.
   */
  def getQuick(column: Int, row: Int): Boolean = {
    QuickBitVector.get(bits, row * columns + column)
  }

  /**
   * Returns a hash code value for the receiver.
   */
  override def hashCode(): Int = toBitVector().hashCode

  /**
   * Performs a logical <b>NOT</b> on the bits of the receiver.
   */
  def not() {
    toBitVector().not()
  }

  /**
   * Performs a logical <b>OR</b> of the receiver with another bit matrix. The
   * receiver is modified so that a bit in it has the value <code>true</code>
   * if and only if it either already had the value <code>true</code> or the
   * corresponding bit in the other bit matrix argument has the value
   * <code>true</code>.
   *
   * @param other
   *            a bit matrix.
   * @throws IllegalArgumentException
   *             if
   *             <tt>columns() != other.columns() || rows() != other.rows()</tt>
   *             .
   */
  def or(other: BitMatrix) {
    checkDimensionCompatibility(other)
    toBitVector().or(other.toBitVector())
  }

  /**
   * Constructs and returns a new matrix with <tt>width</tt> columns and
   * <tt>height</tt> rows which is a copy of the contents of the given box.
   * The box ranges from <tt>[column,row]</tt> to
   * <tt>[column+width-1,row+height-1]</tt>, all inclusive.
   *
   * @param column
   *            the index of the column-coordinate.
   * @param row
   *            the index of the row-coordinate.
   * @param width
   *            the width of the box.
   * @param height
   *            the height of the box.
   * @throws IndexOutOfBoundsException
   *             if
   *             <tt>column&lt;0 || column+width&gt;columns() || row&lt;0 || row+height&gt;rows()</tt>
   */
  def part(column: Int, 
      row: Int, 
      width: Int, 
      height: Int): BitMatrix = {
    if (column < 0 || column + width > columns || row < 0 || row + height > rows) throw new IndexOutOfBoundsException("column:" + column + ", row:" + row + " ,width:" + width + 
      ", height:" + 
      height)
    if (width <= 0 || height <= 0) return new BitMatrix(0, 0)
    val subMatrix = new BitMatrix(width, height)
    subMatrix.replaceBoxWith(0, 0, width, height, this, column, row)
    subMatrix
  }

  /**
   * Sets the bit at the specified coordinate to the state specified by
   * <tt>value</tt>.
   *
   * @param column
   *            the index of the column-coordinate.
   * @param row
   *            the index of the row-coordinate.
   * @param value
   *            the value of the bit to be copied into the specified
   *            coordinate.
   * @throws IndexOutOfBoundsException
   *             if
   *             <tt>column&lt;0 || column&gt;=columns() || row&lt;0 || row&gt;=rows()</tt>
   */
  def put(column: Int, row: Int, value: Boolean) {
    if (column < 0 || column >= columns || row < 0 || row >= rows) throw new IndexOutOfBoundsException("column:" + column + ", row:" + row)
    QuickBitVector.put(bits, row * columns + column, value)
  }

  /**
   * Sets the bit at the specified coordinate to the state specified by
   * <tt>value</tt>; <b>WARNING:</b> Does not check preconditions.
   *
   * <p>
   * Provided with invalid parameters this method may return invalid values
   * without throwing any exception. <b>You should only use this method when
   * you are absolutely sure that the coordinate is within bounds.</b>
   * Precondition (unchecked):
   * <tt>column&gt;=0 && column&lt;columns() && row&gt;=0 && row&lt;rows()</tt>.
   *
   * @param column
   *            the index of the column-coordinate.
   * @param row
   *            the index of the row-coordinate.
   * @param value
   *            the value of the bit to be copied into the specified
   *            coordinate.
   */
  def putQuick(column: Int, row: Int, value: Boolean) {
    QuickBitVector.put(bits, row * columns + column, value)
  }

  /**
   * Replaces a box of the receiver with the contents of another matrix's box.
   * The source box ranges from <tt>[sourceColumn,sourceRow]</tt> to
   * <tt>[sourceColumn+width-1,sourceRow+height-1]</tt>, all inclusive. The
   * destination box ranges from <tt>[column,row]</tt> to
   * <tt>[column+width-1,row+height-1]</tt>, all inclusive. Does nothing if
   * <tt>width &lt;= 0 || height &lt;= 0</tt>. If <tt>source==this</tt> and
   * the source and destination box intersect in an ambiguous way, then
   * replaces as if using an intermediate auxiliary copy of the receiver.
   *
   * @param column
   *            the index of the column-coordinate.
   * @param row
   *            the index of the row-coordinate.
   * @param width
   *            the width of the box.
   * @param height
   *            the height of the box.
   * @param source
   *            the source matrix to copy from(may be identical to the
   *            receiver).
   * @param sourceColumn
   *            the index of the source column-coordinate.
   * @param sourceRow
   *            the index of the source row-coordinate.
   * @throws IndexOutOfBoundsException
   *             if
   *             <tt>column&lt;0 || column+width&gt;columns() || row&lt;0 || row+height&gt;rows()</tt>
   * @throws IndexOutOfBoundsException
   *             if
   *
   *             <tt>sourceColumn&lt;0 || sourceColumn+width&gt;source.columns() || sourceRow&lt;0 || sourceRow+height&gt;source.rows()</tt>
   */
  def replaceBoxWith(column: Int, 
      row: Int, 
      width: Int, 
      height: Int, 
      source: BitMatrix, 
      sourceColumn: Int, 
      sourceRow: Int) {
    this.containsBox(column, row, width, height)
    source.containsBox(sourceColumn, sourceRow, width, height)
    if (width <= 0 || height <= 0) return
    if (source == this) {
      val destRect = new Rectangle(column, row, width, height)
      val sourceRect = new Rectangle(sourceColumn, sourceRow, width, height)
      if (destRect.intersects(sourceRect)) {
        source = source.copy()
      }
    }
    val sourceVector = source.toBitVector()
    val destVector = this.toBitVector()
    val sourceColumns = source.columns()
    while (height >= 0) {
      val offset = row * columns + column
      val sourceOffset = sourceRow * sourceColumns + sourceColumn
      destVector.replaceFromToWith(offset, offset + width - 1, sourceVector, sourceOffset)
      row += 1
      sourceRow += 1
    }
  }

  /**
   * Sets the bits in the given box to the state specified by <tt>value</tt>.
   * The box ranges from <tt>[column,row]</tt> to
   * <tt>[column+width-1,row+height-1]</tt>, all inclusive. (Does nothing if
   * <tt>width &lt;= 0 || height &lt;= 0</tt>).
   *
   * @param column
   *            the index of the column-coordinate.
   * @param row
   *            the index of the row-coordinate.
   * @param width
   *            the width of the box.
   * @param height
   *            the height of the box.
   * @param value
   *            the value of the bit to be copied into the bits of the
   *            specified box.
   * @throws IndexOutOfBoundsException
   *             if
   *             <tt>column&lt;0 || column+width&gt;columns() || row&lt;0 || row+height&gt;rows()</tt>
   */
  def replaceBoxWith(column: Int, 
      row: Int, 
      width: Int, 
      height: Int, 
      value: Boolean) {
    containsBox(column, row, width, height)
    if (width <= 0 || height <= 0) return
    val destVector = this.toBitVector()
    while (height >= 0) {
      val offset = row * columns + column
      destVector.replaceFromToWith(offset, offset + width - 1, value)
      row += 1
    }
  }

  /**
   * Returns the number of rows of the receiver.
   */
  def rows(): Int = rows

  /**
   * Returns the size of the receiver which is <tt>columns()*rows()</tt>.
   */
  def size(): Int = columns * rows

  /**
   * Converts the receiver to a bitvector. In many cases this method only
   * makes sense on one-dimensional matrices. <b>WARNING:</b> The returned
   * bitvector and the receiver share the <b>same</b> backing bits. Modifying
   * either of them will affect the other. If this behaviour is not what you
   * want, you should first use <tt>copy()</tt> to make sure both objects use
   * separate internal storage.
   */
  def toBitVector(): BitVector = new BitVector(bits, size)

  /**
   * Returns a (very crude) string representation of the receiver.
   */
  override def toString(): String = toBitVector().toString

  /**
   * Performs a logical <b>XOR</b> of the receiver with another bit matrix.
   * The receiver is modified so that a bit in it has the value
   * <code>true</code> if and only if one of the following statements holds:
   * <ul>
   * <li>The bit initially has the value <code>true</code>, and the
   * corresponding bit in the argument has the value <code>false</code>.
   * <li>The bit initially has the value <code>false</code>, and the
   * corresponding bit in the argument has the value <code>true</code>.
   * </ul>
   *
   * @param other
   *            a bit matrix.
   * @throws IllegalArgumentException
   *             if
   *             <tt>columns() != other.columns() || rows() != other.rows()</tt>
   *             .
   */
  def xor(other: BitMatrix) {
    checkDimensionCompatibility(other)
    toBitVector().xor(other.toBitVector())
  }
}
