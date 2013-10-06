package cern.jet.math.tint

import cern.jet.math.tdouble.DoubleArithmetic
import cern.jet.random.tdouble.engine.DoubleMersenneTwister
import cern.colt.function.FunctionTypes.{IntIntFunction, IntFunction}
import cern.colt.function.ProcedureTypes.{IntProcedure, IntIntProcedure}


/**
 * Integer Function objects to be passed to generic methods.
 *
 * @author wolfgang.hoschek@cern.ch
 * @version 1.0, 09/24/99
 */
object IntFunctions {

  /**
   * Function that returns <tt>Math.abs(a) == (a < 0) ? -a : a</tt>.
   */
  val abs = new IntFunction() {

    def apply(a: Int): Int = if (a < 0) -a else a
  }

  /**
   * Function that returns <tt>a--</tt>.
   */
  val dec = new IntFunction() {

    def apply(a: Int): Int = a - 1
  }

  /**
   * Function that returns <tt>(int) Arithmetic.factorial(a)</tt>.
   */
  val factorial = new IntFunction() {

    def apply(a: Int): Int = DoubleArithmetic.factorial(a).toInt
  }

  /**
   * Function that returns its argument.
   */
  val identity = new IntFunction() {

    def apply(a: Int): Int = a
  }

  /**
   * Function that returns <tt>a++</tt>.
   */
  val inc = new IntFunction() {

    def apply(a: Int): Int = a + 1
  }

  /**
   * Function that returns <tt>-a</tt>.
   */
  val neg = new IntFunction() {

    def apply(a: Int): Int = -a
  }

  /**
   * Function that returns <tt>~a</tt>.
   */
  val not = new IntFunction() {

    def apply(a: Int): Int = ~a
  }

  /**
   * Function that returns <tt>a < 0 ? -1 : a > 0 ? 1 : 0</tt>.
   */
  val sign = new IntFunction() {

    def apply(a: Int): Int = if (a < 0) -1 else if (a > 0) 1 else 0
  }

  /**
   * Function that returns <tt>a * a</tt>.
   */
  val square = new IntFunction() {

    def apply(a: Int): Int = a * a
  }

  /**
   * Function that returns <tt>a & b</tt>.
   */
  val and = new IntIntFunction() {

    def apply(a: Int, b: Int): Int = a & b
  }

  /**
   * Function that returns <tt>a < b ? -1 : a > b ? 1 : 0</tt>.
   */
  val compare = new IntIntFunction() {

    def apply(a: Int, b: Int): Int = if (a < b) -1 else if (a > b) 1 else 0
  }

  /**
   * Function that returns <tt>a / b</tt>.
   */
  val div = new IntIntFunction() {

    def apply(a: Int, b: Int): Int = a / b
  }

  /**
   * Function that returns <tt>-(a / b)</tt>.
   */
  val divNeg = new IntIntFunction() {

    def apply(a: Int, b: Int): Int = -(a / b)
  }

  /**
   * Function that returns <tt>a == b ? 1 : 0</tt>.
   */
  val equals = new IntIntFunction() {

    def apply(a: Int, b: Int): Int = if (a == b) 1 else 0
  }

  /**
   * Function that returns <tt>a == b</tt>.
   */
  val isEqual = new IntIntProcedure() {

    def apply(a: Int, b: Int): Boolean = a == b
  }

  /**
   * Function that returns <tt>a < b</tt>.
   */
  val isLess = new IntIntProcedure() {

    def apply(a: Int, b: Int): Boolean = a < b
  }

  /**
   * Function that returns <tt>a > b</tt>.
   */
  val isGreater = new IntIntProcedure() {

    def apply(a: Int, b: Int): Boolean = a > b
  }

  /**
   * Function that returns <tt>Math.max(a,b)</tt>.
   */
  val max = new IntIntFunction() {

    def apply(a: Int, b: Int): Int = if (a >= b) a else b
  }

  /**
   * Function that returns <tt>Math.min(a,b)</tt>.
   */
  val min = new IntIntFunction() {

    def apply(a: Int, b: Int): Int = if (a <= b) a else b
  }

  /**
   * Function that returns <tt>a - b</tt>.
   */
  val minus = new IntIntFunction() {

    def apply(a: Int, b: Int): Int = a - b
  }

  /**
   * Function that returns <tt>a % b</tt>.
   */
  val mod = new IntIntFunction() {

    def apply(a: Int, b: Int): Int = a % b
  }

  /**
   * Function that returns <tt>a * b</tt>.
   */
  val mult = new IntIntFunction() {

    def apply(a: Int, b: Int): Int = a * b
  }

  /**
   * Function that returns <tt>-(a * b)</tt>.
   */
  val multNeg = new IntIntFunction() {

    def apply(a: Int, b: Int): Int = -(a * b)
  }

  /**
   * Function that returns <tt>a * b**2</tt>.
   */
  val multSquare = new IntIntFunction() {

    def apply(a: Int, b: Int): Int = a * b * b
  }

  /**
   * Function that returns <tt>a | b</tt>.
   */
  val or = new IntIntFunction() {

    def apply(a: Int, b: Int): Int = a | b
  }

  /**
   * Function that returns <tt>a + b</tt>.
   */
  val plus = new IntIntFunction() {

    def apply(a: Int, b: Int): Int = a + b
  }

  /**
   * Function that returns <tt>Math.abs(a) + Math.abs(b)</tt>.
   */
  val plusAbs = new IntIntFunction() {

    def apply(a: Int, b: Int): Int = Math.abs(a) + Math.abs(b)
  }

  /**
   * Function that returns <tt>(int) Math.pow(a,b)</tt>.
   */
  val pow = new IntIntFunction() {

    def apply(a: Int, b: Int): Int = Math.pow(a, b).toInt
  }

  /**
   * Function that returns <tt>a << b</tt>.
   */
  val shiftLeft = new IntIntFunction() {

    def apply(a: Int, b: Int): Int = a << b
  }

  /**
   * Function that returns <tt>a >> b</tt>.
   */
  val shiftRightSigned = new IntIntFunction() {

    def apply(a: Int, b: Int): Int = a >> b
  }

  /**
   * Function that returns <tt>a >>> b</tt>.
   */
  val shiftRightUnsigned = new IntIntFunction() {

    def apply(a: Int, b: Int): Int = a >>> b
  }

  /**
   * Function that returns <tt>a ** b</tt>.
   */
  val xor = new IntIntFunction() {

    def apply(a: Int, b: Int): Int = a ^ b
  }

  /**
   * Constructs a function that returns <tt>a & b</tt>. <tt>a</tt> is a
   * variable, <tt>b</tt> is fixed.
   */
  def and(b: Int): IntFunction = {
    new IntFunction() {

      def apply(a: Int): Int = a & b
    }
  }

  /**
   * Constructs a function that returns <tt>(from<=a && a<=to) ? 1 : 0</tt>.
   * <tt>a</tt> is a variable, <tt>from</tt> and <tt>to</tt> are fixed.
   */
  def between(from: Int, to: Int): IntFunction = {
    new IntFunction() {

      def apply(a: Int): Int = if (from <= a && a <= to) 1 else 0
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
  def bindArg1(function: IntIntFunction, c: Int): IntFunction = {
    new IntFunction() {

      def apply(`var`: Int): Int = function.apply(c, `var`)
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
  def bindArg2(function: IntIntFunction, c: Int): IntFunction = {
    new IntFunction() {

      def apply(`var`: Int): Int = function.apply(`var`, c)
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
  def chain(g: IntFunction, h: IntFunction): IntFunction = {
    new IntFunction() {

      def apply(a: Int): Int = g.apply(h.apply(a))
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
  def chain(g: IntFunction, h: IntIntFunction): IntIntFunction = {
    new IntIntFunction() {

      def apply(a: Int, b: Int): Int = g.apply(h.apply(a, b))
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
  def chain(f: IntIntFunction, g: IntFunction, h: IntFunction): IntIntFunction = {
    new IntIntFunction() {

      def apply(a: Int, b: Int): Int = f.apply(g.apply(a), h.apply(b))
    }
  }

  /**
   * Constructs a function that returns <tt>a < b ? -1 : a > b ? 1 : 0</tt>.
   * <tt>a</tt> is a variable, <tt>b</tt> is fixed.
   */
  def compare(b: Int): IntFunction = {
    new IntFunction() {

      def apply(a: Int): Int = if (a < b) -1 else if (a > b) 1 else 0
    }
  }

  /**
   * Constructs a function that returns the constant <tt>c</tt>.
   */
  def constant(c: Int): IntFunction = {
    new IntFunction() {

      def apply(a: Int): Int = c
    }
  }

  /**
   * Constructs a function that returns <tt>a / b</tt>. <tt>a</tt> is a
   * variable, <tt>b</tt> is fixed.
   */
  def div(b: Int): IntFunction = {
    new IntFunction() {
      def apply(a: Int): Int = a / b
    }
  }

  /**
   * Constructs a function that returns <tt>a == b ? 1 : 0</tt>. <tt>a</tt> is
   * a variable, <tt>b</tt> is fixed.
   */
  def equals(b: Int): IntFunction = {
    new IntFunction() {

      def apply(a: Int): Int = if (a == b) 1 else 0
    }
  }

  /**
   * Constructs a function that returns <tt>from<=a && a<=to</tt>. <tt>a</tt>
   * is a variable, <tt>from</tt> and <tt>to</tt> are fixed.
   */
  def isBetween(from: Int, to: Int): IntProcedure = {
    new IntProcedure() {

      def apply(a: Int): Boolean = from <= a && a <= to
    }
  }

  /**
   * Constructs a function that returns <tt>a == b</tt>. <tt>a</tt> is a
   * variable, <tt>b</tt> is fixed.
   */
  def isEqual(b: Int): IntProcedure = {
    new IntProcedure() {

      def apply(a: Int): Boolean = a == b
    }
  }

  /**
   * Constructs a function that returns <tt>a > b</tt>. <tt>a</tt> is a
   * variable, <tt>b</tt> is fixed.
   */
  def isGreater(b: Int): IntProcedure = {
    new IntProcedure() {

      def apply(a: Int): Boolean = a > b
    }
  }

  /**
   * Constructs a function that returns <tt>a < b</tt>. <tt>a</tt> is a
   * variable, <tt>b</tt> is fixed.
   */
  def isLess(b: Int): IntProcedure = {
    new IntProcedure() {

      def apply(a: Int): Boolean = a < b
    }
  }

  /**
   * Constructs a function that returns <tt>Math.max(a,b)</tt>. <tt>a</tt> is
   * a variable, <tt>b</tt> is fixed.
   */
  def max(b: Int): IntFunction = {
    new IntFunction() {

      def apply(a: Int): Int = if (a >= b) a else b
    }
  }

  /**
   * Constructs a function that returns <tt>Math.min(a,b)</tt>. <tt>a</tt> is
   * a variable, <tt>b</tt> is fixed.
   */
  def min(b: Int): IntFunction = {
    new IntFunction() {

      def apply(a: Int): Int = if (a <= b) a else b
    }
  }

  /**
   * Constructs a function that returns <tt>a - b</tt>. <tt>a</tt> is a
   * variable, <tt>b</tt> is fixed.
   */
  def minus(b: Int): IntFunction = {
    new IntFunction() {

      def apply(a: Int): Int = a - b
    }
  }

  /**
   * Constructs a function that returns <tt>a - b*constant</tt>. <tt>a</tt>
   * and <tt>b</tt> are variables, <tt>constant</tt> is fixed.
   */
  def minusMult(constant: Int): IntIntFunction = plusMultSecond(-constant)

  /**
   * Constructs a function that returns <tt>a % b</tt>. <tt>a</tt> is a
   * variable, <tt>b</tt> is fixed.
   */
  def mod(b: Int): IntFunction = {
    new IntFunction() {

      def apply(a: Int): Int = a % b
    }
  }

  /**
   * Constructs a function that returns <tt>a * b</tt>. <tt>a</tt> is a
   * variable, <tt>b</tt> is fixed.
   */
  def mult(b: Int): IntFunction = {
    new IntFunction() {
      def apply(a: Int): Int = a * b
    }
  }

  /**
   * Constructs a function that returns <tt>a | b</tt>. <tt>a</tt> is a
   * variable, <tt>b</tt> is fixed.
   */
  def or(b: Int): IntFunction = {
    new IntFunction() {

      def apply(a: Int): Int = a | b
    }
  }

  /**
   * Constructs a function that returns <tt>a + b</tt>. <tt>a</tt> is a
   * variable, <tt>b</tt> is fixed.
   */
  def plus(b: Int): IntFunction = {
    new IntFunction() {

      def apply(a: Int): Int = a + b
    }
  }

  /**
   * Constructs a function that returns <tt>b*constant</tt>.
   */
  def multSecond(constant: Int): IntIntFunction = {
    new IntIntFunction() {

      def apply(a: Int, b: Int): Int = b * constant
    }
  }

  /**
   * Constructs a function that returns <tt>(int) Math.pow(a,b)</tt>.
   * <tt>a</tt> is a variable, <tt>b</tt> is fixed.
   */
  def pow(b: Int): IntFunction = {
    new IntFunction() {

      def apply(a: Int): Int = Math.pow(a, b).toInt
    }
  }

  /**
   * Constructs a function that returns <tt>a + b*constant</tt>. <tt>a</tt>
   * and <tt>b</tt> are variables, <tt>constant</tt> is fixed.
   */
  def plusMultSecond(constant: Int): IntIntFunction = IntPlusMultSecond.plusMult(constant)

  /**
   * Constructs a function that returns <tt>a * constant + b</tt>. <tt>a</tt>
   * and <tt>b</tt> are variables, <tt>constant</tt> is fixed.
   */
  def plusMultFirst(constant: Int): IntIntFunction = new IntPlusMultFirst(constant)

  /**
   * Constructs a function that returns a 32 bit uniformly distributed random
   * number in the closed longerval <tt>[Int.MIN_VALUE,Int.MAX_VALUE]</tt>
   * (including <tt>Int.MIN_VALUE</tt> and <tt>Int.MAX_VALUE</tt>). Currently
   * the engine is
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
  def random(): IntFunction = {
    new IntFunction() {
      val twister = new DoubleMersenneTwister(new java.util.Date())

      def apply(argument: Int) = twister.nextInt()
    }
  }

  /**
   * Constructs a function that returns <tt>a << b</tt>. <tt>a</tt> is a
   * variable, <tt>b</tt> is fixed.
   */
  def shiftLeft(b: Int): IntFunction = {
    new IntFunction() {

      def apply(a: Int): Int = a << b
    }
  }

  /**
   * Constructs a function that returns <tt>a >> b</tt>. <tt>a</tt> is a
   * variable, <tt>b</tt> is fixed.
   */
  def shiftRightSigned(b: Int): IntFunction = {
    new IntFunction() {

      def apply(a: Int): Int = a >> b
    }
  }

  /**
   * Constructs a function that returns <tt>a >>> b</tt>. <tt>a</tt> is a
   * variable, <tt>b</tt> is fixed.
   */
  def shiftRightUnsigned(b: Int): IntFunction = {
    new IntFunction() {

      def apply(a: Int): Int = a >>> b
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
  def swapArgs(function: IntIntFunction): IntIntFunction = {
    new IntIntFunction() {

      def apply(a: Int, b: Int): Int = function.apply(b, a)
    }
  }

  /**
   * Constructs a function that returns <tt>a | b</tt>. <tt>a</tt> is a
   * variable, <tt>b</tt> is fixed.
   */
  def xor(b: Int): IntFunction = {
    new IntFunction() {

      def apply(a: Int): Int = a ^ b
    }
  }
}
