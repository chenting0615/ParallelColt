package cern.jet.math.tfloat

import cern.colt.function.FunctionTypes.{FloatFloatFunction, FloatFunction}
import cern.colt.function.ProcedureTypes.{FloatProcedure, FloatFloatProcedure}


/**
 * Function objects to be passed to generic methods. Contains the functions of
 * java.lang.Math as function objects, as well as a few more basic
 * functions.
 * <p>
 * Function objects conveniently allow to express arbitrary functions in a
 * generic manner. Essentially, a function object is an object that can perform
 * a function on some arguments. It has a minimal interface: a method
 * <tt>apply</tt> that takes the arguments, computes something and returns some
 * result value. Function objects are comparable to function pointers in C used
 * for call-backs.
 * <p>
 * Unary functions are of type cern.colt.function.tfloat.FloatFunction} ,
 * binary functions of type cern.colt.function.tfloat.FloatFloatFunction}
 * . All can be retrieved via <tt>public
 static final</tt> variables named after the function. Unary predicates are of
 * type cern.colt.function.tfloat.FloatProcedure}, binary predicates of
 * typecern.colt.function.tfloat.FloatFloatProcedure}. All can be
 * retrieved via <tt>public
 static final</tt> variables named <tt>isXXX</tt>.
 *
 * <p>
 * Binary functions and predicates also exist as unary functions with the second
 * argument being fixed to a constant. These are generated and retrieved via
 * factory methods (again with the same name as the function). Example:
 * <ul>
 * <li><tt>Functions.pow</tt> gives the function <tt>a<sup>b</sup></tt>.
 * <li><tt>Functions.pow.apply(2,3)==8</tt>.
 * <li><tt>Functions.pow(3)</tt> gives the function <tt>a<sup>3</sup></tt>.
 * <li><tt>Functions.pow(3).apply(2)==8</tt>.
 * </ul>
 * More general, any binary function can be made an unary functions by fixing
 * either the first or the second argument. See methods
 * #bindArg1(FloatFloatFunction,float)} and
 * #bindArg2(FloatFloatFunction,float)}. The order of arguments can be
 * swapped so that the first argument becomes the second and vice-versa. See
 * method #swapArgs(FloatFloatFunction)}. Example:
 * <ul>
 * <li><tt>Functions.pow</tt> gives the function <tt>a<sup>b</sup></tt>.
 * <li><tt>Functions.bindArg2(Functions.pow,3)</tt> gives the function
 * <tt>x<sup>3</sup></tt>.
 * <li><tt>Functions.bindArg1(Functions.pow,3)</tt> gives the function
 * <tt>3<sup>x</sup></tt>.
 * <li><tt>Functions.swapArgs(Functions.pow)</tt> gives the function
 * <tt>b<sup>a</sup></tt>.
 * </ul>
 * <p>
 * Even more general, functions can be chained (composed, assembled). Assume we
 * have two unary functions <tt>g</tt> and <tt>h</tt>. The unary function
 * <tt>g(h(a))</tt> applying both in sequence can be generated via
 * #chain(FloatFunction,FloatFunction)}:
 * <ul>
 * <li><tt>Functions.chain(g,h);</tt>
 * </ul>
 * Assume further we have a binary function <tt>f</tt>. The binary function
 * <tt>g(f(a,b))</tt> can be generated via
 * #chain(FloatFunction,FloatFloatFunction)}:
 * <ul>
 * <li><tt>Functions.chain(g,f);</tt>
 * </ul>
 * The binary function <tt>f(g(a),h(b))</tt> can be generated via
 * #chain(FloatFloatFunction,FloatFunction,FloatFunction)}:
 * <ul>
 * <li><tt>Functions.chain(f,g,h);</tt>
 * </ul>
 * Arbitrarily complex functions can be composed from these building blocks. For
 * example <tt>sin(a) + cos<sup>2</sup>(b)</tt> can be specified as follows:
 * <ul>
 * <li><tt>chain(plus,sin,chain(square,cos));</tt>
 * </ul>
 * or, of course, as
 *
 * <pre>
 * new FloatFloatFunction() {
 *     public final float apply(float a, float b) {
 *         return Math.sin(a) + Math.pow(Math.cos(b), 2);
 *     }
 * }
 * </pre>
 *
 * <p>
 * For aliasing see #functions}. Try this
 * <table>
 * <td class="PRE">
 *
 * <pre>
 * // should yield 1.4399560356056456 in all cases
 * float a = 0.5;
 * float b = 0.2;
 * float v = Math.sin(a) + Math.pow(Math.cos(b), 2);
 * System.out.println(v);
 * Functions F = Functions.functions;
 * FloatFloatFunction f = F.chain(F.plus, F.sin, F.chain(F.square, F.cos));
 * System.out.println(f.apply(a, b));
 * FloatFloatFunction g = new FloatFloatFunction() {
 *     public float apply(float a, float b) {
 *         return Math.sin(a) + Math.pow(Math.cos(b), 2);
 *     }
 * };
 * System.out.println(g.apply(a, b));
 * </pre>
 *
 * </td>
 * </table>
 *
 * <p>
 * <H3>Performance</H3>
 *
 * Surprise. Using modern non-adaptive JITs such as SunJDK 1.2.2 (java -classic)
 * there seems to be no or only moderate performance penalty in using function
 * objects in a loop over traditional code in a loop. For complex nested
 * function objects (e.g.
 * <tt>F.chain(F.abs,F.chain(F.plus,F.sin,F.chain(F.square,F.cos)))</tt>) the
 * penalty is zero, for trivial functions (e.g. <tt>F.plus</tt>) the penalty is
 * often acceptable. <center>
 * <table border cellpadding="3" cellspacing="0" * align="center">
 * <tr valign="middle" bgcolor="#33CC66" nowrap align="center">
 * <td nowrap columnspan="7"><font size="+2">Iteration Performance [million
 * function evaluations per second]</font><br>
 * <font size="-1">Pentium Pro 200 Mhz, SunJDK 1.2.2, NT, java -classic, </font>
 * </td>
 * </tr>
 * <tr valign="middle" bgcolor="#66CCFF" nowrap align="center">
 * <td nowrap bgcolor="#FF9966" rowspan="2">&nbsp;</td>
 * <td bgcolor="#FF9966" columnspan="2">
 * <p>
 * 30000000 iterations
 * </p>
 * </td>
 * <td bgcolor="#FF9966" columnspan="2">3000000 iterations (10 times less)</td>
 * <td bgcolor="#FF9966" columnspan="2">&nbsp;</td>
 * </tr>
 * <tr valign="middle" bgcolor="#66CCFF" nowrap align="center">
 * <td bgcolor="#FF9966"> <tt>F.plus</tt></td>
 * <td bgcolor="#FF9966"><tt>a+b</tt></td>
 * <td bgcolor="#FF9966">
 * <tt>F.chain(F.abs,F.chain(F.plus,F.sin,F.chain(F.square,F.cos)))</tt></td>
 * <td bgcolor="#FF9966">
 * <tt>Math.abs(Math.sin(a) + Math.pow(Math.cos(b),2))</tt></td>
 * <td bgcolor="#FF9966">&nbsp;</td>
 * <td bgcolor="#FF9966">&nbsp;</td>
 * </tr>
 * <tr valign="middle" bgcolor="#66CCFF" nowrap align="center">
 * <td nowrap bgcolor="#FF9966">&nbsp;</td>
 * <td nowrap>10.8</td>
 * <td nowrap>29.6</td>
 * <td nowrap>0.43</td>
 * <td nowrap>0.35</td>
 * <td nowrap>&nbsp;</td>
 * <td nowrap>&nbsp;</td>
 * </tr>
 * </table>
 * </center>
 *
 *
 * @author wolfgang.hoschek@cern.ch
 * @version 1.0, 09/24/99
 */
object FloatFunctions {

  /**
   * Function that returns <tt>Math.abs(a)</tt>.
   */
  val abs = new FloatFunction() {

    def apply(a: Float): Float = Math.abs(a)
  }

  /**
   * Function that returns <tt>Math.acos(a)</tt>.
   */
  val acos = new FloatFunction() {

    def apply(a: Float): Float = Math.acos(a).toFloat
  }

  /**
   * Function that returns <tt>Math.asin(a)</tt>.
   */
  val asin = new FloatFunction() {

    def apply(a: Float): Float = Math.asin(a).toFloat
  }

  /**
   * Function that returns <tt>Math.atan(a)</tt>.
   */
  val atan = new FloatFunction() {

    def apply(a: Float): Float = Math.atan(a).toFloat
  }

  /**
   * Function that returns <tt>Math.ceil(a)</tt>.
   */
  val ceil = new FloatFunction() {

    def apply(a: Float): Float = Math.ceil(a).toFloat
  }

  /**
   * Function that returns <tt>Math.cos(a)</tt>.
   */
  val cos = new FloatFunction() {

    def apply(a: Float): Float = Math.cos(a).toFloat
  }

  /**
   * Function that returns <tt>Math.exp(a)</tt>.
   */
  val exp = new FloatFunction() {

    def apply(a: Float): Float = Math.exp(a).toFloat
  }

  /**
   * Function that returns <tt>Math.floor(a)</tt>.
   */
  val floor = new FloatFunction() {

    def apply(a: Float): Float = Math.floor(a).toFloat
  }

  /**
   * Function that returns its argument.
   */
  val identity = new FloatFunction() {

    def apply(a: Float): Float = a
  }

  /**
   * Function that returns <tt>1.0 / a</tt>.
   */
  val inv = new FloatFunction() {

    def apply(a: Float): Float = (1.0 / a).toFloat
  }

  /**
   * Function that returns <tt>Math.log(a)</tt>.
   */
  val log = new FloatFunction() {

    def apply(a: Float): Float = Math.log(a).toFloat
  }

  /**
   * Function that returns <tt>Math.log(a) / Math.log(2)</tt>.
   */
  val log2 = new FloatFunction() {

    def apply(a: Float): Float = {
      (Math.log(a) * 1.4426950408889634).toFloat
    }
  }

  /**
   * Function that returns <tt>-a</tt>.
   */
  val neg = new FloatFunction() {

    def apply(a: Float): Float = -a
  }

  /**
   * Function that returns <tt>Math.rint(a)</tt>.
   */
  val rint = new FloatFunction() {

    def apply(a: Float): Float = Math.rint(a).toFloat
  }

  /**
   * Function that returns <tt>a < 0 ? -1 : a > 0 ? 1 : 0</tt>.
   */
  val sign = new FloatFunction() {

    def apply(a: Float): Float = if (a < 0) -1 else if (a > 0) 1 else 0
  }

  /**
   * Function that returns <tt>Math.sin(a)</tt>.
   */
  val sin = new FloatFunction() {

    def apply(a: Float): Float = Math.sin(a).toFloat
  }

  /**
   * Function that returns <tt>Math.sqrt(a)</tt>.
   */
  val sqrt = new FloatFunction() {

    def apply(a: Float): Float = Math.sqrt(a).toFloat
  }

  /**
   * Function that returns <tt>a * a</tt>.
   */
  val square = new FloatFunction() {

    def apply(a: Float): Float = a * a
  }

  /**
   * Function that returns <tt>Math.tan(a)</tt>.
   */
  val tan = new FloatFunction() {

    def apply(a: Float): Float = Math.tan(a).toFloat
  }

  /**
   * Function that returns <tt>Math.atan2(a,b)</tt>.
   */
  val atan2 = new FloatFloatFunction() {

    def apply(a: Float, b: Float): Float = Math.atan2(a, b).toFloat
  }

  /**
   * Function that returns <tt>a < b ? -1 : a > b ? 1 : 0</tt>.
   */
  val compare = new FloatFloatFunction() {

    def apply(a: Float, b: Float): Float = if (a < b) -1 else if (a > b) 1 else 0
  }

  /**
   * Function that returns <tt>a / b</tt>.
   */
  val div = new FloatFloatFunction() {

    def apply(a: Float, b: Float): Float = a / b
  }

  /**
   * Function that returns <tt>-(a / b)</tt>.
   */
  val divNeg = new FloatFloatFunction() {

    def apply(a: Float, b: Float): Float = -(a / b)
  }

  /**
   * Function that returns <tt>a == b ? 1 : 0</tt>.
   */
  val equals = new FloatFloatFunction() {

    def apply(a: Float, b: Float): Float = if (a == b) 1 else 0
  }

  /**
   * Function that returns <tt>a > b ? 1 : 0</tt>.
   */
  val greater = new FloatFloatFunction() {

    def apply(a: Float, b: Float): Float = if (a > b) 1 else 0
  }

  /**
   * Function that returns <tt>Math.IEEEremainder(a,b)</tt>.
   */
  val IEEEremainder = new FloatFloatFunction() {

    def apply(a: Float, b: Float): Float = Math.IEEEremainder(a, b).toFloat
  }

  /**
   * Function that returns <tt>a == b</tt>.
   */
  val isEqual = new FloatFloatProcedure() {

    def apply(a: Float, b: Float): Boolean = a == b
  }

  /**
   * Function that returns <tt>a < b</tt>.
   */
  val isLess = new FloatFloatProcedure() {

    def apply(a: Float, b: Float): Boolean = a < b
  }

  /**
   * Function that returns <tt>a > b</tt>.
   */
  val isGreater = new FloatFloatProcedure() {

    def apply(a: Float, b: Float): Boolean = a > b
  }

  /**
   * Function that returns <tt>a < b ? 1 : 0</tt>.
   */
  val less = new FloatFloatFunction() {

    def apply(a: Float, b: Float): Float = if (a < b) 1 else 0
  }

  /**
   * Function that returns <tt>Math.log(a) / Math.log(b)</tt>.
   */
  val lg = new FloatFloatFunction() {

    def apply(a: Float, b: Float): Float = (Math.log(a) / Math.log(b)).toFloat
  }

  /**
   * Function that returns <tt>Math.max(a,b)</tt>.
   */
  val max = new FloatFloatFunction() {

    def apply(a: Float, b: Float): Float = Math.max(a, b)
  }

  /**
   * Function that returns <tt>Math.min(a,b)</tt>.
   */
  val min = new FloatFloatFunction() {

    def apply(a: Float, b: Float): Float = Math.min(a, b)
  }

  /**
   * Function that returns <tt>a - b</tt>.
   */
  val minus = plusMultSecond(-1)

  /**
   * Function that returns <tt>a % b</tt>.
   */
  val mod = new FloatFloatFunction() {

    def apply(a: Float, b: Float): Float = a % b
  }

  /**
   * Function that returns <tt>a * b</tt>.
   */
  val mult = new FloatFloatFunction() {

    def apply(a: Float, b: Float): Float = a * b
  }

  /**
   * Function that returns <tt>-(a * b)</tt>.
   */
  val multNeg = new FloatFloatFunction() {

    def apply(a: Float, b: Float): Float = -(a * b)
  }

  /**
   * Function that returns <tt>a * b**2</tt>.
   */
  val multSquare = new FloatFloatFunction() {

    def apply(a: Float, b: Float): Float = a * b * b
  }

  /**
   * Function that returns <tt>a + b</tt>.
   */
  val plus = plusMultSecond(1)

  /**
   * Function that returns <tt>Math.abs(a) + Math.abs(b)</tt>.
   */
  val plusAbs = new FloatFloatFunction() {

    def apply(a: Float, b: Float): Float = Math.abs(a) + Math.abs(b)
  }

  /**
   * Function that returns <tt>Math.pow(a,b)</tt>.
   */
  val pow = new FloatFloatFunction() {

    def apply(a: Float, b: Float): Float = Math.pow(a, b).toFloat
  }

  /**
   * Constructs a function that returns <tt>(from<=a && a<=to) ? 1 : 0</tt>.
   * <tt>a</tt> is a variable, <tt>from</tt> and <tt>to</tt> are fixed.
   */
  def between(from: Float, to: Float): FloatFunction = {
    new FloatFunction() {

      def apply(a: Float): Float = if (from <= a && a <= to) 1 else 0
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
  def bindArg1(function: FloatFloatFunction, c: Float): FloatFunction = {
    new FloatFunction() {

      def apply(`var`: Float): Float = function.apply(c, `var`)
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
  def bindArg2(function: FloatFloatFunction, c: Float): FloatFunction = {
    new FloatFunction() {

      def apply(`var`: Float): Float = function.apply(`var`, c)
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
  def chain(f: FloatFloatFunction, g: FloatFunction, h: FloatFunction): FloatFloatFunction = {
    new FloatFloatFunction() {

      def apply(a: Float, b: Float): Float = f.apply(g.apply(a), h.apply(b))
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
  def chain(g: FloatFunction, h: FloatFloatFunction): FloatFloatFunction = {
    new FloatFloatFunction() {

      def apply(a: Float, b: Float): Float = g.apply(h.apply(a, b))
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
  def chain(g: FloatFunction, h: FloatFunction): FloatFunction = {
    new FloatFunction() {

      def apply(a: Float): Float = g.apply(h.apply(a))
    }
  }

  /**
   * Constructs a function that returns <tt>a < b ? -1 : a > b ? 1 : 0</tt>.
   * <tt>a</tt> is a variable, <tt>b</tt> is fixed.
   */
  def compare(b: Float): FloatFunction = {
    new FloatFunction() {

      def apply(a: Float): Float = if (a < b) -1 else if (a > b) 1 else 0
    }
  }

  /**
   * Constructs a function that returns the constant <tt>c</tt>.
   */
  def constant(c: Float): FloatFunction = {
    new FloatFunction() {

      def apply(a: Float): Float = c
    }
  }

  /**
   * Demonstrates usage of this class.
   */
  def demo1() {
    val a = 0.5f
    val b = 0.2f
    val v = (Math.sin(a) + Math.pow(Math.cos(b), 2)).toFloat
    println(v)
    val f = FloatFunctions.chain(FloatFunctions.plus, FloatFunctions.sin, FloatFunctions.chain(FloatFunctions.square,
      FloatFunctions.cos))
    println(f.apply(a, b))
    val g = new FloatFloatFunction() {

      def apply(x: Float, y: Float): Float = {
        (Math.sin(x) + Math.pow(Math.cos(y), 2)).toFloat
      }
    }
    println(g.apply(a, b))
    val m = FloatFunctions.plus(3)
    val n = FloatFunctions.plus(4)
    println(m.apply(0))
    println(n.apply(0))
  }

  /**
   * Benchmarks and demonstrates usage of trivial and complex functions.
   */
  def demo2(size: Int) {
    println("\n\n")
    var a = 0.0f
    var b = 0.0f
    val v = Math.abs(Math.sin(a) + Math.pow(Math.cos(b), 2)).toFloat
    println(v)
    val f = FloatFunctions.chain(FloatFunctions.abs, FloatFunctions.chain(FloatFunctions.plus, FloatFunctions.sin,
      FloatFunctions.chain(FloatFunctions.square, FloatFunctions.cos)))
    println(f.apply(a, b))
    val g = new FloatFloatFunction() {

      def apply(x: Float, y: Float): Float = {
        Math.abs(Math.sin(x) + Math.pow(Math.cos(y), 2)).toFloat
      }
    }
    println(g.apply(a, b))
    val emptyLoop = new cern.colt.Timer().start()
    a = 0
    b = 0
    var sum = 0f
    var i = size
    while (i >= 0) {
      sum += a
      a += 1
      b += 1
      i -= 1
    }
    emptyLoop.stop().display()
    println("empty sum=" + sum)
    val timer = new cern.colt.Timer().start()
    a = 0
    b = 0
    sum = 0f
    i = size
    while (i >= 0) {
      sum += Math.abs(Math.sin(a) + Math.pow(Math.cos(b), 2)).toFloat
      a += 1
      b += 1
      i -= 1
    }
    timer.stop().display()
    println("evals / sec = " + size / timer.minus(emptyLoop).seconds())
    println("sum=" + sum)
    timer.reset().start()
    a = 0
    b = 0
    sum = 0
    i = size
    while (i >= 0) {
      sum += f.apply(a, b)
      a += 1
      b += 1
      i -= 1
    }
    timer.stop().display()
    println("evals / sec = " + size / timer.minus(emptyLoop).seconds())
    println("sum=" + sum)
    timer.reset().start()
    a = 0
    b = 0
    sum = 0
    i = size
    while (i >= 0) {
      sum += g.apply(a, b)
      a += 1
      b += 1
      i -= 1
    }
    timer.stop().display()
    println("evals / sec = " + size / timer.minus(emptyLoop).seconds())
    println("sum=" + sum)
  }

  /**
   * Constructs a function that returns <tt>a / b</tt>. <tt>a</tt> is a
   * variable, <tt>b</tt> is fixed.
   */
  def div(b: Float): FloatFunction = mult(1 / b)

  /**
   * Constructs a function that returns <tt>a == b ? 1 : 0</tt>. <tt>a</tt> is
   * a variable, <tt>b</tt> is fixed.
   */
  def equals(b: Float): FloatFunction = {
    new FloatFunction() {

      def apply(a: Float): Float = if (a == b) 1 else 0
    }
  }

  /**
   * Constructs a function that returns <tt>a > b ? 1 : 0</tt>. <tt>a</tt> is
   * a variable, <tt>b</tt> is fixed.
   */
  def greater(b: Float): FloatFunction = {
    new FloatFunction() {

      def apply(a: Float): Float = if (a > b) 1 else 0
    }
  }

  /**
   * Constructs a function that returns <tt>Math.IEEEremainder(a,b)</tt>.
   * <tt>a</tt> is a variable, <tt>b</tt> is fixed.
   */
  def IEEEremainder(b: Float): FloatFunction = {
    new FloatFunction() {

      def apply(a: Float): Float = Math.IEEEremainder(a, b).toFloat
    }
  }

  /**
   * Constructs a function that returns <tt>from<=a && a<=to</tt>. <tt>a</tt>
   * is a variable, <tt>from</tt> and <tt>to</tt> are fixed.
   */
  def isBetween(from: Float, to: Float): FloatProcedure = {
    new FloatProcedure() {

      def apply(a: Float): Boolean = from <= a && a <= to
    }
  }

  /**
   * Constructs a function that returns <tt>a == b</tt>. <tt>a</tt> is a
   * variable, <tt>b</tt> is fixed.
   */
  def isEqual(b: Float): FloatProcedure = {
    new FloatProcedure() {

      def apply(a: Float): Boolean = a == b
    }
  }

  /**
   * Constructs a function that returns <tt>a > b</tt>. <tt>a</tt> is a
   * variable, <tt>b</tt> is fixed.
   */
  def isGreater(b: Float): FloatProcedure = {
    new FloatProcedure() {

      def apply(a: Float): Boolean = a > b
    }
  }

  /**
   * Constructs a function that returns <tt>a < b</tt>. <tt>a</tt> is a
   * variable, <tt>b</tt> is fixed.
   */
  def isLess(b: Float): FloatProcedure = {
    new FloatProcedure() {

      def apply(a: Float): Boolean = a < b
    }
  }

  /**
   * Constructs a function that returns <tt>a < b ? 1 : 0</tt>. <tt>a</tt> is
   * a variable, <tt>b</tt> is fixed.
   */
  def less(b: Float): FloatFunction = {
    new FloatFunction() {

      def apply(a: Float): Float = if (a < b) 1 else 0
    }
  }

  /**
   * Constructs a function that returns <tt><tt>Math.log(a) / Math.log(b)</tt>
   * </tt>. <tt>a</tt> is a variable, <tt>b</tt> is fixed.
   */
  def lg(b: Float): FloatFunction = {
    new FloatFunction() {

      private val logInv = 1 / Math.log(b)

      def apply(a: Float): Float = (Math.log(a) * logInv).toFloat
    }
  }

  /**
   * Tests various methods of this class.
   */
  protected def main(args: Array[String]) {
    val size = Integer.parseInt(args(0))
    demo2(size)
  }

  /**
   * Constructs a function that returns <tt>Math.max(a,b)</tt>. <tt>a</tt> is
   * a variable, <tt>b</tt> is fixed.
   */
  def max(b: Float): FloatFunction = {
    new FloatFunction() {

      def apply(a: Float): Float = Math.max(a, b)
    }
  }

  /**
   * Constructs a function that returns <tt>Math.min(a,b)</tt>. <tt>a</tt> is
   * a variable, <tt>b</tt> is fixed.
   */
  def min(b: Float): FloatFunction = {
    new FloatFunction() {

      def apply(a: Float): Float = Math.min(a, b)
    }
  }

  /**
   * Constructs a function that returns <tt>a - b</tt>. <tt>a</tt> is a
   * variable, <tt>b</tt> is fixed.
   */
  def minus(b: Float): FloatFunction = plus(-b)

  /**
   * Constructs a function that returns <tt>a - b*constant</tt>. <tt>a</tt>
   * and <tt>b</tt> are variables, <tt>constant</tt> is fixed.
   */
  def minusMult(constant: Float): FloatFloatFunction = plusMultSecond(-constant)

  /**
   * Constructs a function that returns <tt>a % b</tt>. <tt>a</tt> is a
   * variable, <tt>b</tt> is fixed.
   */
  def mod(b: Float): FloatFunction = {
    new FloatFunction() {

      def apply(a: Float): Float = a % b
    }
  }

  /**
   * Constructs a function that returns <tt>a * b</tt>. <tt>a</tt> is a
   * variable, <tt>b</tt> is fixed.
   */
  def mult(b: Float): FloatFunction = new FloatMult(b)

  /**
   * Constructs a function that returns <tt>a + b</tt>. <tt>a</tt> is a
   * variable, <tt>b</tt> is fixed.
   */
  def plus(b: Float): FloatFunction = {
    new FloatFunction() {

      def apply(a: Float): Float = a + b
    }
  }

  def multSecond(constant: Float): FloatFloatFunction = {
    new FloatFloatFunction() {

      def apply(a: Float, b: Float): Float = b * constant
    }
  }

  /**
   * Constructs a function that returns <tt>a + b*constant</tt>. <tt>a</tt>
   * and <tt>b</tt> are variables, <tt>constant</tt> is fixed.
   */
  def plusMultSecond(constant: Float): FloatFloatFunction = new FloatPlusMultSecond(constant)

  /**
   * Constructs a function that returns <tt>a * constant + b</tt>. <tt>a</tt>
   * and <tt>b</tt> are variables, <tt>constant</tt> is fixed.
   */
  def plusMultFirst(constant: Float): FloatFloatFunction = new FloatPlusMultFirst(constant)

  /**
   * Constructs a function that returns <tt>Math.pow(a,b)</tt>. <tt>a</tt> is
   * a variable, <tt>b</tt> is fixed.
   */
  def pow(b: Float): FloatFunction = {
    new FloatFunction() {

      def apply(a: Float): Float = Math.pow(a, b).toFloat
    }
  }

  /**
   * Constructs a function that returns a new uniform random number in the
   * open unit interval <code>(0.0,1.0)</code> (excluding 0.0 and 1.0).
   * Currently the engine is
   * cern.jet.random.tfloat.engine.FloatMersenneTwister} and is seeded
   * with the current time.
   * <p>
   * Note that any random engine derived from
   * cern.jet.random.tfloat.engine.FloatRandomEngine} and any random
   * distribution derived from
   * cern.jet.random.tfloat.AbstractFloatDistribution} are function
   * objects, because they implement the proper interfaces. Thus, if you are
   * not happy with the default, just pass your favourite random generator to
   * function evaluating methods.
   */
  def random(): FloatFunction = new RandomFloatFunction()

  private class RandomFloatFunction extends FloatFunction {

    def apply(argument: Float): Float = math.random.toFloat
  }

  /**
   * Constructs a function that returns the number rounded to the given
   * precision; <tt>Math.rint(a/precision)*precision</tt>. Examples:
   *
   * <pre>
   * precision = 0.01 rounds 0.012 --&gt; 0.01, 0.018 --&gt; 0.02
   * precision = 10   rounds 123   --&gt; 120 , 127   --&gt; 130
   * </pre>
   */
  def round(precision: Float): FloatFunction = {
    new FloatFunction() {

      def apply(a: Float): Float = {
        (Math.rint(a / precision) * precision).toFloat
      }
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
  def swapArgs(function: FloatFloatFunction): FloatFloatFunction = {
    new FloatFloatFunction() {

      def apply(a: Float, b: Float): Float = function.apply(b, a)
    }
  }
}
