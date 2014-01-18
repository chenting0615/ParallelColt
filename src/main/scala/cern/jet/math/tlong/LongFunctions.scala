package cern.jet.math.tlong

import cern.jet.math.tdouble.DoubleArithmetic
import cern.jet.random.tdouble.engine.DoubleMersenneTwister


/**
 * Long Function objects to be passed to generic methods. Same as
 * DoubleFunctions except operating on longs.
 *
 * @author wolfgang.hoschek@cern.ch
 * @version 1.0, 09/24/99
 */
object LongFunctions {

  type LongFunction = Function1[Long, Long]
  type LongLongFunction = Function2[Long, Long, Long]
  type LongProcedure = Function1[Long, Boolean]
  type LongLongProcedure = Function2[Long, Long, Boolean]

  /**
   * Function that returns <tt>Math.abs(a) == (a < 0) ? -a : a</tt>.
   */
  val abs = new LongFunction() {

    def apply(a: Long): Long = if (a < 0) -a else a
  }

  /**
   * Function that returns <tt>a--</tt>.
   */
  val dec = new LongFunction() {

    def apply(a: Long): Long = a - 1
  }

  /**
   * Function that returns <tt>(long) Arithmetic.factorial(a)</tt>.
   */
  val factorial = new LongFunction() {

    def apply(a: Long): Long = DoubleArithmetic.factorial(a).toLong
  }

  /**
   * Function that returns its argument.
   */
  val identity = new LongFunction() {

    def apply(a: Long): Long = a
  }

  /**
   * Function that returns <tt>a++</tt>.
   */
  val inc = new LongFunction() {

    def apply(a: Long): Long = a + 1
  }

  /**
   * Function that returns <tt>-a</tt>.
   */
  val neg = new LongFunction() {

    def apply(a: Long): Long = -a
  }

  /**
   * Function that returns <tt>~a</tt>.
   */
  val not = new LongFunction() {

    def apply(a: Long): Long = ~a
  }

  /**
   * Function that returns <tt>a < 0 ? -1 : a > 0 ? 1 : 0</tt>.
   */
  val sign = new LongFunction() {

    def apply(a: Long): Long = if (a < 0) -1 else if (a > 0) 1 else 0
  }

  /**
   * Function that returns <tt>a * a</tt>.
   */
  val square = new LongFunction() {

    def apply(a: Long): Long = a * a
  }

  /**
   * Function that returns <tt>a & b</tt>.
   */
  val and = new LongLongFunction() {

    def apply(a: Long, b: Long): Long = a & b
  }

  /**
   * Function that returns <tt>a < b ? -1 : a > b ? 1 : 0</tt>.
   */
  val compare = new LongLongFunction() {

    def apply(a: Long, b: Long): Long = if (a < b) -1 else if (a > b) 1 else 0
  }

  /**
   * Function that returns <tt>a / b</tt>.
   */
  val div = new LongLongFunction() {

    def apply(a: Long, b: Long): Long = a / b
  }

  /**
   * Function that returns <tt>-(a / b)</tt>.
   */
  val divNeg = new LongLongFunction() {

    def apply(a: Long, b: Long): Long = -(a / b)
  }

  /**
   * Function that returns <tt>a == b ? 1 : 0</tt>.
   */
  val equals = new LongLongFunction() {

    def apply(a: Long, b: Long): Long = if (a == b) 1 else 0
  }

  /**
   * Function that returns <tt>a == b</tt>.
   */
  val isEqual = new LongLongProcedure() {

    def apply(a: Long, b: Long): Boolean = a == b
  }

  /**
   * Function that returns <tt>a < b</tt>.
   */
  val isLess = new LongLongProcedure() {

    def apply(a: Long, b: Long): Boolean = a < b
  }

  /**
   * Function that returns <tt>a > b</tt>.
   */
  val isGreater = new LongLongProcedure() {

    def apply(a: Long, b: Long): Boolean = a > b
  }

  /**
   * Function that returns <tt>Math.max(a,b)</tt>.
   */
  val max = new LongLongFunction() {

    def apply(a: Long, b: Long): Long = if (a >= b) a else b
  }

  /**
   * Function that returns <tt>Math.min(a,b)</tt>.
   */
  val min = new LongLongFunction() {

    def apply(a: Long, b: Long): Long = if (a <= b) a else b
  }

  /**
   * Function that returns <tt>a - b</tt>.
   */
  val minus = new LongLongFunction() {

    def apply(a: Long, b: Long): Long = a - b
  }

  /**
   * Function that returns <tt>a % b</tt>.
   */
  val mod = new LongLongFunction() {

    def apply(a: Long, b: Long): Long = a % b
  }

  /**
   * Function that returns <tt>a * b</tt>.
   */
  val mult = new LongLongFunction() {

    def apply(a: Long, b: Long): Long = a * b
  }

  /**
   * Function that returns <tt>-(a * b)</tt>.
   */
  val multNeg = new LongLongFunction() {

    def apply(a: Long, b: Long): Long = -(a * b)
  }

  /**
   * Function that returns <tt>a * b**2</tt>.
   */
  val multSquare = new LongLongFunction() {

    def apply(a: Long, b: Long): Long = a * b * b
  }

  /**
   * Function that returns <tt>a | b</tt>.
   */
  val or = new LongLongFunction() {

    def apply(a: Long, b: Long): Long = a | b
  }

  /**
   * Function that returns <tt>a + b</tt>.
   */
  val plus = new LongLongFunction() {

    def apply(a: Long, b: Long): Long = a + b
  }

  /**
   * Function that returns <tt>Math.abs(a) + Math.abs(b)</tt>.
   */
  val plusAbs = new LongLongFunction() {

    def apply(a: Long, b: Long): Long = Math.abs(a) + Math.abs(b)
  }

  /**
   * Function that returns <tt>(long) Math.pow(a,b)</tt>.
   */
  val pow = new LongLongFunction() {

    def apply(a: Long, b: Long): Long = Math.pow(a, b).toLong
  }

  /**
   * Function that returns <tt>a << b</tt>.
   */
  val shiftLeft = new LongLongFunction() {

    def apply(a: Long, b: Long): Long = a << b
  }

  /**
   * Function that returns <tt>a >> b</tt>.
   */
  val shiftRightSigned = new LongLongFunction() {

    def apply(a: Long, b: Long): Long = a >> b
  }

  /**
   * Function that returns <tt>a >>> b</tt>.
   */
  val shiftRightUnsigned = new LongLongFunction() {

    def apply(a: Long, b: Long): Long = a >>> b
  }

  /**
   * Function that returns <tt>a ** b</tt>.
   */
  val xor = new LongLongFunction() {

    def apply(a: Long, b: Long): Long = a ^ b
  }

  /**
   * Constructs a function that returns <tt>a & b</tt>. <tt>a</tt> is a
   * variable, <tt>b</tt> is fixed.
   */
  def and(b: Long): LongFunction = {
    new LongFunction() {

      def apply(a: Long): Long = a & b
    }
  }

  /**
   * Constructs a function that returns <tt>(from<=a && a<=to) ? 1 : 0</tt>.
   * <tt>a</tt> is a variable, <tt>from</tt> and <tt>to</tt> are fixed.
   */
  def between(from: Long, to: Long): LongFunction = {
    new LongFunction() {

      def apply(a: Long): Long = if (from <= a && a <= to) 1 else 0
    }
  }

  /**
   * Constructs a unary function from a binary function with the first operand
   * (argument) fixed to the given constant <tt>c</tt>. The second operand is
   * variable (free).
   *
   * @param function
   *            a binary function taking operands in the form
   *            <tt>function.apply(c,var)</tt>.
   * @return the unary function <tt>function(c,var)</tt>.
   */
  def bindArg1(function: LongLongFunction, c: Long): LongFunction = {
    new LongFunction() {

      def apply(`var`: Long): Long = function.apply(c, `var`)
    }
  }

  /**
   * Constructs a unary function from a binary function with the second
   * operand (argument) fixed to the given constant <tt>c</tt>. The first
   * operand is variable (free).
   *
   * @param function
   *            a binary function taking operands in the form
   *            <tt>function.apply(var,c)</tt>.
   * @return the unary function <tt>function(var,c)</tt>.
   */
  def bindArg2(function: LongLongFunction, c: Long): LongFunction = {
    new LongFunction() {

      def apply(`var`: Long): Long = function.apply(`var`, c)
    }
  }

  /**
   * Constructs the function <tt>g( h(a) )</tt>.
   *
   * @param g
   *            a unary function.
   * @param h
   *            a unary function.
   * @return the unary function <tt>g( h(a) )</tt>.
   */
  def chain(g: LongFunction, h: LongFunction): LongFunction = {
    new LongFunction() {

      def apply(a: Long): Long = g.apply(h.apply(a))
    }
  }

  /**
   * Constructs the function <tt>g( h(a,b) )</tt>.
   *
   * @param g
   *            a unary function.
   * @param h
   *            a binary function.
   * @return the unary function <tt>g( h(a,b) )</tt>.
   */
  def chain(g: LongFunction, h: LongLongFunction): LongLongFunction = {
    new LongLongFunction() {

      def apply(a: Long, b: Long): Long = g.apply(h.apply(a, b))
    }
  }

  /**
   * Constructs the function <tt>f( g(a), h(b) )</tt>.
   *
   * @param f
   *            a binary function.
   * @param g
   *            a unary function.
   * @param h
   *            a unary function.
   * @return the binary function <tt>f( g(a), h(b) )</tt>.
   */
  def chain(f: LongLongFunction, g: LongFunction, h: LongFunction): LongLongFunction = {
    new LongLongFunction() {

      def apply(a: Long, b: Long): Long = f.apply(g.apply(a), h.apply(b))
    }
  }

  /**
   * Constructs a function that returns <tt>a < b ? -1 : a > b ? 1 : 0</tt>.
   * <tt>a</tt> is a variable, <tt>b</tt> is fixed.
   */
  def compare(b: Long): LongFunction = {
    new LongFunction() {

      def apply(a: Long): Long = if (a < b) -1 else if (a > b) 1 else 0
    }
  }

  /**
   * Constructs a function that returns the constant <tt>c</tt>.
   */
  def constant(c: Long): LongFunction = {
    new LongFunction() {

      def apply(a: Long): Long = c
    }
  }

  /**
   * Constructs a function that returns <tt>a / b</tt>. <tt>a</tt> is a
   * variable, <tt>b</tt> is fixed.
   */
  def div(b: Long): LongFunction = {
    new LongFunction() {

      def apply(a: Long): Long = a / b
    }
  }

  /**
   * Constructs a function that returns <tt>a == b ? 1 : 0</tt>. <tt>a</tt> is
   * a variable, <tt>b</tt> is fixed.
   */
  def equals(b: Long): LongFunction = {
    new LongFunction() {

      def apply(a: Long): Long = if (a == b) 1 else 0
    }
  }

  /**
   * Constructs a function that returns <tt>from<=a && a<=to</tt>. <tt>a</tt>
   * is a variable, <tt>from</tt> and <tt>to</tt> are fixed.
   */
  def isBetween(from: Long, to: Long): LongProcedure = {
    new LongProcedure() {

      def apply(a: Long): Boolean = from <= a && a <= to
    }
  }

  /**
   * Constructs a function that returns <tt>a == b</tt>. <tt>a</tt> is a
   * variable, <tt>b</tt> is fixed.
   */
  def isEqual(b: Long): LongProcedure = {
    new LongProcedure() {

      def apply(a: Long): Boolean = a == b
    }
  }

  /**
   * Constructs a function that returns <tt>a > b</tt>. <tt>a</tt> is a
   * variable, <tt>b</tt> is fixed.
   */
  def isGreater(b: Long): LongProcedure = {
    new LongProcedure() {

      def apply(a: Long): Boolean = a > b
    }
  }

  /**
   * Constructs a function that returns <tt>a < b</tt>. <tt>a</tt> is a
   * variable, <tt>b</tt> is fixed.
   */
  def isLess(b: Long): LongProcedure = {
    new LongProcedure() {

      def apply(a: Long): Boolean = a < b
    }
  }

  /**
   * Constructs a function that returns <tt>Math.max(a,b)</tt>. <tt>a</tt> is
   * a variable, <tt>b</tt> is fixed.
   */
  def max(b: Long): LongFunction = {
    new LongFunction() {

      def apply(a: Long): Long = if (a >= b) a else b
    }
  }

  /**
   * Constructs a function that returns <tt>Math.min(a,b)</tt>. <tt>a</tt> is
   * a variable, <tt>b</tt> is fixed.
   */
  def min(b: Long): LongFunction = {
    new LongFunction() {

      def apply(a: Long): Long = if (a <= b) a else b
    }
  }

  /**
   * Constructs a function that returns <tt>a - b</tt>. <tt>a</tt> is a
   * variable, <tt>b</tt> is fixed.
   */
  def minus(b: Long): LongFunction = {
    new LongFunction() {

      def apply(a: Long): Long = a - b
    }
  }

  /**
   * Constructs a function that returns <tt>a - b*constant</tt>. <tt>a</tt>
   * and <tt>b</tt> are variables, <tt>constant</tt> is fixed.
   */
  def minusMult(constant: Long): LongLongFunction = plusMultSecond(-constant)

  /**
   * Constructs a function that returns <tt>a % b</tt>. <tt>a</tt> is a
   * variable, <tt>b</tt> is fixed.
   */
  def mod(b: Long): LongFunction = {
    new LongFunction() {

      def apply(a: Long): Long = a % b
    }
  }

  /**
   * Constructs a function that returns <tt>a * b</tt>. <tt>a</tt> is a
   * variable, <tt>b</tt> is fixed.
   */
  def mult(b: Long): LongFunction = {
    new LongFunction() {

      def apply(a: Long): Long = a * b
    }
  }

  /**
   * Constructs a function that returns <tt>a | b</tt>. <tt>a</tt> is a
   * variable, <tt>b</tt> is fixed.
   */
  def or(b: Long): LongFunction = {
    new LongFunction() {

      def apply(a: Long): Long = a | b
    }
  }

  /**
   * Constructs a function that returns <tt>a + b</tt>. <tt>a</tt> is a
   * variable, <tt>b</tt> is fixed.
   */
  def plus(b: Long): LongFunction = {
    new LongFunction() {

      def apply(a: Long): Long = a + b
    }
  }

  /**
   * Constructs a function that returns <tt>b*constant</tt>.
   */
  def multSecond(constant: Long): LongLongFunction = {
    new LongLongFunction() {

      def apply(a: Long, b: Long): Long = b * constant
    }
  }

  /**
   * Constructs a function that returns <tt>(long) Math.pow(a,b)</tt>.
   * <tt>a</tt> is a variable, <tt>b</tt> is fixed.
   */
  def pow(b: Long): LongFunction = {
    new LongFunction() {

      def apply(a: Long): Long = Math.pow(a, b).toLong
    }
  }

  /**
   * Constructs a function that returns <tt>a + b*constant</tt>. <tt>a</tt>
   * and <tt>b</tt> are variables, <tt>constant</tt> is fixed.
   */
  def plusMultSecond(constant: Long): LongLongFunction = new LongPlusMultSecond(constant)

  /**
   * Constructs a function that returns <tt>a * constant + b</tt>. <tt>a</tt>
   * and <tt>b</tt> are variables, <tt>constant</tt> is fixed.
   */
  def plusMultFirst(constant: Long): LongLongFunction = new LongPlusMultFirst(constant)

  /**
   * Constructs a function that returns a 32 bit uniformly distributed random
   * number in the closed longerval <tt>[Long.MIN_VALUE,Long.MAX_VALUE]</tt>
   * (including <tt>Long.MIN_VALUE</tt> and <tt>Long.MAX_VALUE</tt>).
   * Currently the engine is
   * cern.jet.random.tdouble.engine.DoubleMersenneTwister and is
   * seeded with the current time.
   * <p>
   * Note that any random engine derived from
   * cern.jet.random.tdouble.engine.DoubleRandomEngine and any random
   * distribution derived from
   * cern.jet.random.tdouble.AbstractDoubleDistribution are function
   * objects, because they implement the proper longerfaces. Thus, if you are
   * not happy with the default, just pass your favourite random generator to
   * function evaluating methods.
   */
  def random(): LongFunction = {
    new LongFunction() {
      private val rndm = new DoubleMersenneTwister(new java.util.Date())

      def apply(a: Long): Long = rndm.nextLong()
    }
  }

  /**
   * Constructs a function that returns <tt>a << b</tt>. <tt>a</tt> is a
   * variable, <tt>b</tt> is fixed.
   */
  def shiftLeft(b: Long): LongFunction = {
    new LongFunction() {

      def apply(a: Long): Long = a << b
    }
  }

  /**
   * Constructs a function that returns <tt>a >> b</tt>. <tt>a</tt> is a
   * variable, <tt>b</tt> is fixed.
   */
  def shiftRightSigned(b: Long): LongFunction = {
    new LongFunction() {

      def apply(a: Long): Long = a >> b
    }
  }

  /**
   * Constructs a function that returns <tt>a >>> b</tt>. <tt>a</tt> is a
   * variable, <tt>b</tt> is fixed.
   */
  def shiftRightUnsigned(b: Long): LongFunction = {
    new LongFunction() {

      def apply(a: Long): Long = a >>> b
    }
  }

  /**
   * Constructs a function that returns <tt>function.apply(b,a)</tt>, i.e.
   * applies the function with the first operand as second operand and the
   * second operand as first operand.
   *
   * @param function
   *            a function taking operands in the form
   *            <tt>function.apply(a,b)</tt>.
   * @return the binary function <tt>function(b,a)</tt>.
   */
  def swapArgs(function: LongLongFunction): LongLongFunction = {
    new LongLongFunction() {

      def apply(a: Long, b: Long): Long = function.apply(b, a)
    }
  }

  /**
   * Constructs a function that returns <tt>a | b</tt>. <tt>a</tt> is a
   * variable, <tt>b</tt> is fixed.
   */
  def xor(b: Long): LongFunction = {
    new LongFunction() {

      def apply(a: Long): Long = a ^ b
    }
  }
}
