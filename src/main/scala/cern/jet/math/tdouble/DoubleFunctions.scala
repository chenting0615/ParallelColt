package cern.jet.math.tdouble

import cern.colt.function.Procedure1
import cern.colt.function.FunctionTypes.DoubleDoubleFunction
import cern.colt.function.ProcedureTypes.DoubleDoubleProcedure

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
 * Unary functions are of type cern.colt.tdouble.DoubleFunction
 * , binary functions of type
 * cern.colt.tdouble.DoubleDoubleFunction. All can be retrieved
 * via <tt>public
 static final</tt> variables named after the function. Unary predicates are of
 * type cern.colt.tdouble.DoubleProcedure, binary predicates of
 * type cern.colt.tdouble.DoubleDoubleProcedure. All can be
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
 * bindArg1(DoubleDoubleFunction,double) and
 * bindArg2(DoubleDoubleFunction,double). The order of arguments can be
 * swapped so that the first argument becomes the second and vice-versa. See
 * method swapArgs(DoubleDoubleFunction). Example:
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
 * chain(DoubleFunction,DoubleFunction)
 * <ul>
 * <li><tt>Functions.chain(g,h);</tt>
 * </ul>
 * Assume further we have a binary function <tt>f</tt>. The binary function
 * <tt>g(f(a,b))</tt> can be generated via
 * chain(DoubleFunction,DoubleDoubleFunction)
 * <ul>
 * <li><tt>Functions.chain(g,f);</tt>
 * </ul>
 * The binary function <tt>f(g(a),h(b))</tt> can be generated via
 * chain(DoubleDoubleFunction,DoubleFunction,DoubleFunction):
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
 * new DoubleDoubleFunction() {
 *     public final double apply(double a, double b) {
 *         return Math.sin(a) + Math.pow(Math.cos(b), 2);
 *     }
 * }
 * </pre>
 *
 * <table>
 * <td class="PRE">
 *
 * <pre>
 * // should yield 1.4399560356056456 in all cases
 * double a = 0.5;
 * double b = 0.2;
 * double v = Math.sin(a) + Math.pow(Math.cos(b), 2);
 * System.out.println(v);
 * Functions F = Functions.functions;
 * DoubleDoubleFunction f = F.chain(F.plus, F.sin, F.chain(F.square, F.cos));
 * System.out.println(f.apply(a, b));
 * DoubleDoubleFunction g = new DoubleDoubleFunction() {
 *     public double apply(double a, double b) {
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
object DoubleFunctions {

  /**
   * Function that returns <tt>Math.abs(a)</tt>.
   */
  val abs = new Function1[Double, Double]() {

    def apply(a: Double): Double = Math.abs(a)
  }

  /**
   * Function that returns <tt>Math.acos(a)</tt>.
   */
  val acos = new Function1[Double, Double]() {

    def apply(a: Double): Double = Math.acos(a)
  }

  /**
   * Function that returns <tt>Math.asin(a)</tt>.
   */
  val asin = new Function1[Double, Double]() {

    def apply(a: Double): Double = Math.asin(a)
  }

  /**
   * Function that returns <tt>Math.atan(a)</tt>.
   */
  val atan = new Function1[Double, Double]() {

    def apply(a: Double): Double = Math.atan(a)
  }

  /**
   * Function that returns <tt>Math.ceil(a)</tt>.
   */
  val ceil = new Function1[Double, Double]() {

    def apply(a: Double): Double = Math.ceil(a)
  }

  /**
   * Function that returns <tt>Math.cos(a)</tt>.
   */
  val cos = new Function1[Double, Double]() {

    def apply(a: Double): Double = Math.cos(a)
  }

  /**
   * Function that returns <tt>Math.exp(a)</tt>.
   */
  val exp = new Function1[Double, Double]() {

    def apply(a: Double): Double = Math.exp(a)
  }

  /**
   * Function that returns <tt>Math.floor(a)</tt>.
   */
  val floor = new Function1[Double, Double]() {

    def apply(a: Double): Double = Math.floor(a)
  }

  /**
   * Function that returns its argument.
   */
  val identity = new Function1[Double, Double]() {

    def apply(a: Double): Double = a
  }

  /**
   * Function that returns <tt>1.0 / a</tt>.
   */
  val inv = new Function1[Double, Double]() {

    def apply(a: Double): Double = 1.0 / a
  }

  /**
   * Function that returns <tt>Math.log(a)</tt>.
   */
  val log = new Function1[Double, Double]() {

    def apply(a: Double): Double = Math.log(a)
  }

  /**
   * Function that returns <tt>Math.log(a) / Math.log(2)</tt>.
   */
  val log2 = new Function1[Double, Double]() {

    def apply(a: Double): Double = Math.log(a) * 1.4426950408889634
  }

  /**
   * Function that returns <tt>-a</tt>.
   */
  val neg = new Function1[Double, Double]() {

    def apply(a: Double): Double = -a
  }

  /**
   * Function that returns <tt>Math.rint(a)</tt>.
   */
  val rint = new Function1[Double, Double]() {

    def apply(a: Double): Double = Math.rint(a)
  }

  /**
   * Function that returns <tt>a < 0 ? -1 : a > 0 ? 1 : 0</tt>.
   */
  val sign = new Function1[Double, Double]() {

    def apply(a: Double): Double = if (a < 0) -1 else if (a > 0) 1 else 0
  }

  /**
   * Function that returns <tt>Math.sin(a)</tt>.
   */
  val sin = new Function1[Double, Double]() {

    def apply(a: Double): Double = Math.sin(a)
  }

  /**
   * Function that returns <tt>Math.sqrt(a)</tt>.
   */
  val sqrt = new Function1[Double, Double]() {

    def apply(a: Double): Double = Math.sqrt(a)
  }

  /**
   * Function that returns <tt>a * a</tt>.
   */
  val square = new Function1[Double, Double]() {

    def apply(a: Double): Double = a * a
  }

  /**
   * Function that returns <tt>Math.tan(a)</tt>.
   */
  val tan = new Function1[Double, Double]() {

    def apply(a: Double): Double = Math.tan(a)
  }

  /**
   * Function that returns <tt>Math.atan2(a,b)</tt>.
   */
  val atan2 = new DoubleDoubleFunction() {

    def apply(a: Double, b: Double): Double = Math.atan2(a, b)
  }

  /**
   * Function that returns <tt>a < b ? -1 : a > b ? 1 : 0</tt>.
   */
  val compare = new DoubleDoubleFunction() {

    def apply(a: Double, b: Double): Double = if (a < b) -1 else if (a > b) 1 else 0
  }

  /**
   * Function that returns <tt>a / b</tt>.
   */
  val div = new DoubleDoubleFunction() {

    def apply(a: Double, b: Double): Double = a / b
  }

  /**
   * Function that returns <tt>-(a / b)</tt>.
   */
  val divNeg = new DoubleDoubleFunction() {

    def apply(a: Double, b: Double): Double = -(a / b)
  }

  /**
   * Function that returns <tt>a == b ? 1 : 0</tt>.
   */
  val equals = new DoubleDoubleFunction() {

    def apply(a: Double, b: Double): Double = if (a == b) 1 else 0
  }

  /**
   * Function that returns <tt>a > b ? 1 : 0</tt>.
   */
  val greater = new DoubleDoubleFunction() {

    def apply(a: Double, b: Double): Double = if (a > b) 1 else 0
  }

  /**
   * Function that returns <tt>Math.IEEEremainder(a,b)</tt>.
   */
  val IEEEremainder = new DoubleDoubleFunction() {

    def apply(a: Double, b: Double): Double = Math.IEEEremainder(a, b)
  }

  /**
   * Function that returns <tt>a == b</tt>.
   */
  val isEqual = new DoubleDoubleProcedure() {

    def apply(a: Double, b: Double): Boolean = a == b
  }

  /**
   * Function that returns <tt>a < b</tt>.
   */
  val isLess = new DoubleDoubleProcedure() {

    def apply(a: Double, b: Double): Boolean = a < b
  }

  /**
   * Function that returns <tt>a > b</tt>.
   */
  val isGreater = new DoubleDoubleProcedure() {

    def apply(a: Double, b: Double): Boolean = a > b
  }

  /**
   * Function that returns <tt>a < b ? 1 : 0</tt>.
   */
  val less = new DoubleDoubleFunction() {

    def apply(a: Double, b: Double): Double = if (a < b) 1 else 0
  }

  /**
   * Function that returns <tt>Math.log(a) / Math.log(b)</tt>.
   */
  val lg = new DoubleDoubleFunction() {

    def apply(a: Double, b: Double): Double = Math.log(a) / Math.log(b)
  }

  /**
   * Function that returns <tt>Math.max(a,b)</tt>.
   */
  val max = new DoubleDoubleFunction() {

    def apply(a: Double, b: Double): Double = Math.max(a, b)
  }

  /**
   * Function that returns <tt>Math.min(a,b)</tt>.
   */
  val min = new DoubleDoubleFunction() {

    def apply(a: Double, b: Double): Double = Math.min(a, b)
  }

  /**
   * Function that returns <tt>a - b</tt>.
   */
  val minus = plusMultSecond(-1)

  /**
   * Function that returns <tt>a % b</tt>.
   */
  val mod = new DoubleDoubleFunction() {

    def apply(a: Double, b: Double): Double = a % b
  }

  /**
   * Function that returns <tt>a * b</tt>.
   */
  val mult = new DoubleDoubleFunction() {

    def apply(a: Double, b: Double): Double = a * b
  }

  /**
   * Function that returns <tt>-(a * b)</tt>.
   */
  val multNeg = new DoubleDoubleFunction() {

    def apply(a: Double, b: Double): Double = -(a * b)
  }

  /**
   * Function that returns <tt>a * b**2</tt>.
   */
  val multSquare = new DoubleDoubleFunction() {

    def apply(a: Double, b: Double): Double = a * b * b
  }

  /**
   * Function that returns <tt>a + b</tt>.
   */
  val plus = plusMultSecond(1)

  /**
   * Function that returns <tt>Math.abs(a) + Math.abs(b)</tt>.
   */
  val plusAbs = new DoubleDoubleFunction() {

    def apply(a: Double, b: Double): Double = Math.abs(a) + Math.abs(b)
  }

  /**
   * Function that returns <tt>Math.pow(a,b)</tt>.
   */
  val pow = new DoubleDoubleFunction() {

    def apply(a: Double, b: Double): Double = Math.pow(a, b)
  }

  /**
   * Returns sqrt(a^2 + b^2) without under/overflow.
   */
  val hypot =  new DoubleDoubleFunction() {
    def apply(a: Double, b: Double) = {
      if (Math.abs(a) > Math.abs(b)) {
        val r = b / a
        Math.abs(a) * Math.sqrt(1 + r * r)
      }
      else if (b != 0) {
        val r = a / b
        Math.abs(b) * Math.sqrt(1 + r * r)
      }
      else
        0.0
    }
  }

  /**
   * Constructs a function that returns <tt>(from<=a && a<=to) ? 1 : 0</tt>.
   * <tt>a</tt> is a variable, <tt>from</tt> and <tt>to</tt> are fixed.
   */
  def between(from: Double, to: Double): Function1[Double, Double] = {
    new Function1[Double, Double]() {

      def apply(a: Double): Double = if (from <= a && a <= to) 1 else 0
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
  def bindArg1(function: DoubleDoubleFunction, c: Double): Function1[Double, Double] = {
    new Function1[Double, Double]() {

      def apply(`var`: Double): Double = function.apply(c, `var`)
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
  def bindArg2(function: DoubleDoubleFunction, c: Double): Function1[Double, Double] = {
    new Function1[Double, Double]() {

      def apply(`var`: Double): Double = function.apply(`var`, c)
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
  def chain(f: DoubleDoubleFunction, g: Function1[Double, Double], h: Function1[Double, Double]): DoubleDoubleFunction = {
    new DoubleDoubleFunction() {

      def apply(a: Double, b: Double): Double = f.apply(g.apply(a), h.apply(b))
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
  def chain(g: Function1[Double, Double], h: DoubleDoubleFunction): DoubleDoubleFunction = {
    new DoubleDoubleFunction() {

      def apply(a: Double, b: Double): Double = g.apply(h.apply(a, b))
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
  def chain(g: Function1[Double, Double], h: Function1[Double, Double]): Function1[Double, Double] = {
    new Function1[Double, Double]() {

      def apply(a: Double): Double = g.apply(h.apply(a))
    }
  }

  /**
   * Constructs a function that returns <tt>a < b ? -1 : a > b ? 1 : 0</tt>.
   * <tt>a</tt> is a variable, <tt>b</tt> is fixed.
   */
  def compare(b: Double): Function1[Double, Double] = {
    new Function1[Double, Double]() {

      def apply(a: Double): Double = if (a < b) -1 else if (a > b) 1 else 0
    }
  }

  /**
   * Constructs a function that returns the constant <tt>c</tt>.
   */
  def constant(c: Double): Function1[Double, Double] = {
    new Function1[Double, Double]() {

      def apply(a: Double): Double = c
    }
  }

  /**
   * Demonstrates usage of this class.
   */
  def demo1() {
    val a = 0.5
    val b = 0.2
    val v = Math.sin(a) + Math.pow(Math.cos(b), 2)
    println(v)
    val f = DoubleFunctions.chain(DoubleFunctions.plus, DoubleFunctions.sin, DoubleFunctions.chain(DoubleFunctions.square,
      DoubleFunctions.cos))
    println(f.apply(a, b))
    val g = new DoubleDoubleFunction() {

      def apply(x: Double, y: Double): Double = {
        Math.sin(x) + Math.pow(Math.cos(y), 2)
      }
    }
    println(g.apply(a, b))
    val m = DoubleFunctions.plus(3)
    val n = DoubleFunctions.plus(4)
    println(m.apply(0))
    println(n.apply(0))
  }

  /**
   * Benchmarks and demonstrates usage of trivial and complex functions.
   */
  def demo2(size: Int) {
    println("\n\n")
    var a = 0.0
    var b = 0.0
    val v = Math.abs(Math.sin(a) + Math.pow(Math.cos(b), 2))
    println(v)
    val f = DoubleFunctions.chain(DoubleFunctions.abs, DoubleFunctions.chain(DoubleFunctions.plus, DoubleFunctions.sin,
      DoubleFunctions.chain(DoubleFunctions.square, DoubleFunctions.cos)))
    println(f.apply(a, b))
    val g = new DoubleDoubleFunction() {

      def apply(x: Double, y: Double): Double = {
        Math.abs(Math.sin(x) + Math.pow(Math.cos(y), 2))
      }
    }
    println(g.apply(a, b))
    val emptyLoop = new cern.colt.Timer().start()
    a = 0
    b = 0
    var sum: Double = 0
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
    sum = 0
    i = size
    while (i >= 0) {
      sum += Math.abs(Math.sin(a) + Math.pow(Math.cos(b), 2))
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
  def div(constant: Double): Function1[Double, Double] = new Function1[Double, Double]() {
    def apply(a: Double) = a / constant
  }

  /**
   * Constructs a function that returns <tt>a == b ? 1 : 0</tt>. <tt>a</tt> is
   * a variable, <tt>b</tt> is fixed.
   */
  def equals(b: Double): Function1[Double, Double] = {
    new Function1[Double, Double]() {

      def apply(a: Double): Double = if (a == b) 1 else 0
    }
  }

  /**
   * Constructs a function that returns <tt>a > b ? 1 : 0</tt>. <tt>a</tt> is
   * a variable, <tt>b</tt> is fixed.
   */
  def greater(b: Double): Function1[Double, Double] = {
    new Function1[Double, Double]() {

      def apply(a: Double): Double = if (a > b) 1 else 0
    }
  }

  /**
   * Constructs a function that returns <tt>Math.IEEEremainder(a,b)</tt>.
   * <tt>a</tt> is a variable, <tt>b</tt> is fixed.
   */
  def IEEEremainder(b: Double): Function1[Double, Double] = {
    new Function1[Double, Double]() {

      def apply(a: Double): Double = Math.IEEEremainder(a, b)
    }
  }

  /**
   * Constructs a function that returns <tt>from<=a && a<=to</tt>. <tt>a</tt>
   * is a variable, <tt>from</tt> and <tt>to</tt> are fixed.
   */
  def isBetween(from: Double, to: Double): Procedure1[Double] = {
    new Procedure1[Double]() {
      def apply(a: Double): Boolean = from <= a && a <= to
    }
  }

  /**
   * Constructs a function that returns <tt>a == b</tt>. <tt>a</tt> is a
   * variable, <tt>b</tt> is fixed.
   */
  def isEqual(b: Double): Procedure1[Double] = {
    new Procedure1[Double]() {
      def apply(a: Double): Boolean = a == b
    }
  }

  /**
   * Constructs a function that returns <tt>a > b</tt>. <tt>a</tt> is a
   * variable, <tt>b</tt> is fixed.
   */
  def isGreater(b: Double): Procedure1[Double] = {
    new Procedure1[Double]() {
      def apply(a: Double): Boolean = a > b
    }
  }

  /**
   * Constructs a function that returns <tt>a < b</tt>. <tt>a</tt> is a
   * variable, <tt>b</tt> is fixed.
   */
  def isLess(b: Double): Procedure1[Double] = {
    new Procedure1[Double]() {
      def apply(a: Double): Boolean = a < b
    }
  }

  /**
   * Constructs a function that returns <tt>a < b ? 1 : 0</tt>. <tt>a</tt> is
   * a variable, <tt>b</tt> is fixed.
   */
  def less(b: Double): Function1[Double, Double] = {
    new Function1[Double, Double]() {
      def apply(a: Double): Double = if (a < b) 1 else 0
    }
  }

  /**
   * Constructs a function that returns <tt><tt>Math.log(a) / Math.log(b)</tt>
   * </tt>. <tt>a</tt> is a variable, <tt>b</tt> is fixed.
   */
  def lg(b: Double): Function1[Double, Double] = {
    new Function1[Double, Double]() {

      private val logInv = 1 / Math.log(b)

      def apply(a: Double): Double = Math.log(a) * logInv
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
  def max(b: Double): Function1[Double, Double] = {
    new Function1[Double, Double]() {

      def apply(a: Double): Double = Math.max(a, b)
    }
  }

  /**
   * Constructs a function that returns <tt>Math.min(a,b)</tt>. <tt>a</tt> is
   * a variable, <tt>b</tt> is fixed.
   */
  def min(b: Double): Function1[Double, Double] = {
    new Function1[Double, Double]() {

      def apply(a: Double): Double = Math.min(a, b)
    }
  }

  /**
   * Constructs a function that returns <tt>a - b</tt>. <tt>a</tt> is a
   * variable, <tt>b</tt> is fixed.
   */
  def minus(b: Double): Function1[Double, Double] = plus(-b)

  /**
   * Constructs a function that returns <tt>a - b*constant</tt>. <tt>a</tt>
   * and <tt>b</tt> are variables, <tt>constant</tt> is fixed.
   */
  def minusMult(constant: Double): DoubleDoubleFunction = plusMultSecond(-constant)

  /**
   * Constructs a function that returns <tt>a % b</tt>. <tt>a</tt> is a
   * variable, <tt>b</tt> is fixed.
   */
  def mod(b: Double): Function1[Double, Double] = {
    new Function1[Double, Double]() {
      def apply(a: Double): Double = a % b
    }
  }

  /**
   * Constructs a function that returns <tt>a * b</tt>. <tt>a</tt> is a
   * variable, <tt>b</tt> is fixed.
   */
  def mult(constant: Double): Function1[Double, Double] = new Function1[Double, Double]() {
      def apply(a: Double) = a * constant
    }

  /**
   * Constructs a function that returns <tt>a + b</tt>. <tt>a</tt> is a
   * variable, <tt>b</tt> is fixed.
   */
  def plus(b: Double): Function1[Double, Double] = {
    new Function1[Double, Double]() {

      def apply(a: Double): Double = a + b
    }
  }

  /**
   * Constructs a function that returns <tt>b*constant</tt>.
   */
  def multSecond(constant: Double): DoubleDoubleFunction = {
    new DoubleDoubleFunction() {

      def apply(a: Double, b: Double): Double = b * constant
    }
  }

  /**
   * Constructs a function that returns <tt>a + b*constant</tt>. <tt>a</tt>
   * and <tt>b</tt> are variables, <tt>constant</tt> is fixed.
   */
  def plusMultSecond(constant: Double): DoubleDoubleFunction = new DoublePlusMultSecond(constant)

  /**
   * Constructs a function that returns <tt>a * constant + b</tt>. <tt>a</tt>
   * and <tt>b</tt> are variables, <tt>constant</tt> is fixed.
   */
  def plusMultFirst(constant: Double): DoubleDoubleFunction = new DoublePlusMultFirst(constant)

  /**
   * Constructs a function that returns <tt>Math.pow(a,b)</tt>. <tt>a</tt> is
   * a variable, <tt>b</tt> is fixed.
   */
  def pow(b: Double): Function1[Double, Double] = {
    new Function1[Double, Double]() {

      def apply(a: Double): Double = Math.pow(a, b)
    }
  }

  /**
   * Constructs a function that returns a new uniform random number in the
   * open unit interval <code>(0.0,1.0)</code> (excluding 0.0 and 1.0).
   * Currently the engine is
   * cern.jet.random.tdouble.engine.DoubleMersenneTwister and is
   * seeded with the current time.
   * <p>
   * Note that any random engine derived from
   * cern.jet.random.tdouble.engine.DoubleRandomEngine and any random
   * distribution derived from
   * cern.jet.random.tdouble.AbstractDoubleDistribution are function
   * objects, because they implement the proper interfaces. Thus, if you are
   * not happy with the default, just pass your favourite random generator to
   * function evaluating methods.
   */
  def random(): Function1[Double, Double] = new Function1[Double, Double] {
    def apply(argument: Double): Double = Math.random()
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
  def round(precision: Double): Function1[Double, Double] = {
    new Function1[Double, Double]() {
      def apply(a: Double): Double = Math.rint(a / precision) * precision
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
  def swapArgs(function: DoubleDoubleFunction): DoubleDoubleFunction = {
    new DoubleDoubleFunction() {
      def apply(a: Double, b: Double): Double = function.apply(b, a)
    }
  }
}
