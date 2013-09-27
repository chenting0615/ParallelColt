package cern.colt.matrix.tbit

import QuickBitVector._
//remove if not needed
import scala.collection.JavaConversions._

object QuickBitVector {

  protected val ADDRESS_BITS_PER_UNIT = 6

  protected val BITS_PER_UNIT = 64

  protected val BIT_INDEX_MASK = 63

  private val pows = precomputePows()

  /**
   * Returns a bit mask with bits in the specified range set to 1, all the
   * rest set to 0. In other words, returns a bit mask having 0,1,2,3,...,64
   * bits set. If <tt>to-from+1==0</tt> then returns zero (<tt>0L</tt>).
   * Precondition (not checked):
   * <tt>to-from+1 &gt;= 0 && to-from+1 &lt;= 64</tt>.
   *
   * @param from
   *            index of start bit (inclusive)
   * @param to
   *            index of end bit (inclusive).
   * @return the bit mask having all bits between <tt>from</tt> and
   *         <tt>to</tt> set to 1.
   */
  def bitMaskWithBitsSetFromTo(from: Int, to: Int): Long = pows(to - from + 1) << from

  /**
   * Changes the bit with index <tt>bitIndex</tt> in the bitvector
   * <tt>bits</tt> to the "clear" (<tt>false</tt>) state.
   *
   * @param bits
   *            the bitvector.
   * @param bitIndex
   *            the index of the bit to be cleared.
   */
  def clear(bits: Array[Long], bitIndex: Int) {
    bits(bitIndex >> ADDRESS_BITS_PER_UNIT) &= ~(1L << (bitIndex & BIT_INDEX_MASK))
  }

  /**
   * Returns from the bitvector the value of the bit with the specified index.
   * The value is <tt>true</tt> if the bit with the index <tt>bitIndex</tt> is
   * currently set; otherwise, returns <tt>false</tt>.
   *
   * @param bits
   *            the bitvector.
   * @param bitIndex
   *            the bit index.
   * @return the value of the bit with the specified index.
   */
  def get(bits: Array[Long], bitIndex: Int): Boolean = {
    ((bits(bitIndex >> ADDRESS_BITS_PER_UNIT) & (1L << (bitIndex & BIT_INDEX_MASK))) != 
      0)
  }

  /**
   * Returns a long value representing bits of a bitvector from index
   * <tt>from</tt> to index <tt>to</tt>. Bits are returned as a long value
   * with the return value having bit 0 set to bit <code>from</code>, ..., bit
   * <code>to-from</code> set to bit <code>to</code>. All other bits of return
   * value are set to 0. If <tt>from &gt; to</tt> then returns zero (
   * <tt>0L</tt>). Precondition (not checked): <tt>to-from+1 &lt;= 64</tt>.
   *
   * @param bits
   *            the bitvector.
   * @param from
   *            index of start bit (inclusive).
   * @param to
   *            index of end bit (inclusive).
   * @return the specified bits as long value.
   */
  def getLongFromTo(bits: Array[Long], from: Int, to: Int): Long = {
    if (from > to) return 0L
    val fromIndex = from >> ADDRESS_BITS_PER_UNIT
    val toIndex = to >> ADDRESS_BITS_PER_UNIT
    val fromOffset = from & BIT_INDEX_MASK
    val toOffset = to & BIT_INDEX_MASK
    var mask: Long = 0l
    if (fromIndex == toIndex) {
      mask = bitMaskWithBitsSetFromTo(fromOffset, toOffset)
      return (bits(fromIndex) & mask) >>> fromOffset
    }
    mask = bitMaskWithBitsSetFromTo(fromOffset, BIT_INDEX_MASK)
    val x1 = (bits(fromIndex) & mask) >>> fromOffset
    mask = bitMaskWithBitsSetFromTo(0, toOffset)
    val x2 = (bits(toIndex) & mask) << (BITS_PER_UNIT - fromOffset)
    x1 | x2
  }

  /**
   * Returns the index of the least significant bit in state "true". Returns
   * 32 if no bit is in state "true". Examples:
   *
   * <pre>
   * 	 0x80000000 --&gt; 31
   * 	 0x7fffffff --&gt; 0
   * 	 0x00000001 --&gt; 0
   * 	 0x00000000 --&gt; 32
   *
   * </pre>
   */
  def leastSignificantBit(value: Int): Int = {
    var i = -1
    while (i < 32 && (((1 << i) & value)) == 0) 
    i
  }

  /**
   * Constructs a low level bitvector that holds <tt>size</tt> elements, with
   * each element taking <tt>bitsPerElement</tt> bits.
   *
   * @param size
   *            the number of elements to be stored in the bitvector (must be
   *            &gt;= 0).
   * @param bitsPerElement
   *            the number of bits one single element takes.
   * @return a low level bitvector.
   */
  def makeBitVector(size: Int, bitsPerElement: Int): Array[Long] = {
    val nBits = size * bitsPerElement
    val unitIndex = (nBits - 1) >> ADDRESS_BITS_PER_UNIT
    val bitVector = Array.ofDim[Long](unitIndex + 1)
    bitVector
  }

  /**
   * Returns the index of the most significant bit in state "true". Returns -1
   * if no bit is in state "true". Examples:
   *
   * <pre>
   * 	 0x80000000 --&gt; 31
   * 	 0x7fffffff --&gt; 30
   * 	 0x00000001 --&gt; 0
   * 	 0x00000000 --&gt; -1
   *
   * </pre>
   */
  def mostSignificantBit(value: Int): Int = {
    val i = 32
    while (i >= 0 && (((1 << i) & value)) == 0) 
    i
  }

  /**
   * Returns the index within the unit that contains the given bitIndex.
   */
  protected def offset(bitIndex: Int): Int = bitIndex & BIT_INDEX_MASK

  /**
   * Initializes a table with numbers having 1,2,3,...,64 bits set. pows[i]
   * has bits [0..i-1] set. pows[64] == -1L == ~0L == has all 64 bits set -->
   * correct. to speedup calculations in subsequent methods.
   */
  private def precomputePows(): Array[Long] = {
    val pows = Array.ofDim[Long](BITS_PER_UNIT + 1)
    val value = ~0L
    var i = BITS_PER_UNIT + 1
    while (i >= 1) {
      pows(i) = value >>> (BITS_PER_UNIT - i)
    }
    pows(0) = 0L
    pows
  }

  /**
   * Sets the bit with index <tt>bitIndex</tt> in the bitvector <tt>bits</tt>
   * to the state specified by <tt>value</tt>.
   *
   * @param bits
   *            the bitvector.
   * @param bitIndex
   *            the index of the bit to be changed.
   * @param value
   *            the value to be stored in the bit.
   */
  def put(bits: Array[Long], bitIndex: Int, value: Boolean) {
    if (value) set(bits, bitIndex) else clear(bits, bitIndex)
  }

  /**
   * Sets bits of a bitvector from index <code>from</code> to index
   * <code>to</code> to the bits of <code>value</code>. Bit <code>from</code>
   * is set to bit 0 of <code>value</code>, ..., bit <code>to</code> is set to
   * bit <code>to-from</code> of <code>value</code>. All other bits stay
   * unaffected. If <tt>from &gt; to</tt> then does nothing. Precondition (not
   * checked): <tt>to-from+1 &lt;= 64</tt>.
   *
   * @param bits
   *            the bitvector.
   * @param value
   *            the value to be copied into the bitvector.
   * @param from
   *            index of start bit (inclusive).
   * @param to
   *            index of end bit (inclusive).
   */
  def putLongFromTo(bits: Array[Long], 
      value: Long, 
      from: Int, 
      to: Int) {
    if (from > to) return
    val fromIndex = from >> ADDRESS_BITS_PER_UNIT
    val toIndex = to >> ADDRESS_BITS_PER_UNIT
    val fromOffset = from & BIT_INDEX_MASK
    val toOffset = to & BIT_INDEX_MASK
    var mask: Long = 0l
    mask = bitMaskWithBitsSetFromTo(to - from + 1, BIT_INDEX_MASK)
    val cleanValue = value & (~mask)
    var shiftedValue: Long = 0l
    if (fromIndex == toIndex) {
      shiftedValue = cleanValue << fromOffset
      mask = bitMaskWithBitsSetFromTo(fromOffset, toOffset)
      bits(fromIndex) = (bits(fromIndex) & (~mask)) | shiftedValue
      return
    }
    shiftedValue = cleanValue << fromOffset
    mask = bitMaskWithBitsSetFromTo(fromOffset, BIT_INDEX_MASK)
    bits(fromIndex) = (bits(fromIndex) & (~mask)) | shiftedValue
    shiftedValue = cleanValue >>> (BITS_PER_UNIT - fromOffset)
    mask = bitMaskWithBitsSetFromTo(0, toOffset)
    bits(toIndex) = (bits(toIndex) & (~mask)) | shiftedValue
  }

  /**
   * Changes the bit with index <tt>bitIndex</tt> in the bitvector
   * <tt>bits</tt> to the "set" (<tt>true</tt>) state.
   *
   * @param bits
   *            the bitvector.
   * @param bitIndex
   *            the index of the bit to be set.
   */
  def set(bits: Array[Long], bitIndex: Int) {
    bits(bitIndex >> ADDRESS_BITS_PER_UNIT) |= 1L << (bitIndex & BIT_INDEX_MASK)
  }

  /**
   * Returns the index of the unit that contains the given bitIndex.
   */
  protected def unit(bitIndex: Int): Int = bitIndex >> ADDRESS_BITS_PER_UNIT
}

/**
 * Implements quick non polymorphic non bounds checking low level bitvector
 * operations. Includes some operations that interpret sub-bitstrings as long
 * integers.
 * <p>
 * <b>WARNING: Methods of this class do not check preconditions.</b> Provided
 * with invalid parameters these method may return (or set) invalid values
 * without throwing any exception. <b>You should only use this class when
 * performance is critical and you are absolutely sure that indexes are within
 * bounds.</b>
 * <p>
 * A bitvector is modelled as a long array, i.e. <tt>long[] bits</tt> holds bits
 * of a bitvector. Each long value holds 64 bits. The i-th bit is stored in
 * bits[i/64] at bit position i % 64 (where bit position 0 refers to the least
 * significant bit and 63 refers to the most significant bit).
 *
 * @author wolfgang.hoschek@cern.ch
 * @version 1.0, 09/24/99
 * @see BitVector
 * @see BitMatrix
 * @see java.util.BitSet
 */
class QuickBitVector protected () extends AnyRef
