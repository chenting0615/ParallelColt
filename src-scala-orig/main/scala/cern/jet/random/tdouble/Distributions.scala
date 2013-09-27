package cern.jet.random.tdouble

import cern.jet.random.tdouble.engine.DoubleRandomEngine
import Distributions._
//remove if not needed
import scala.collection.JavaConversions._

object Distributions {

  /**
   * Returns the probability distribution function of the discrete geometric
   * distribution.
   * <p>
   * <tt>p(k) = p * (1-p)^k</tt> for <tt> k &gt;= 0</tt>.
   * <p>
   *
   * @param k
   *            the argument to the probability distribution function.
   * @param p
   *            the parameter of the probability distribution function.
   */
  def geometricPdf(k: Int, p: Double): Double = {
    if (k < 0) throw new IllegalArgumentException()
    p * Math.pow(1 - p, k)
  }

  /**
   * Returns a random number from the Burr II, VII, VIII, X Distributions.
   * <p>
   * <b>Implementation:</b> Inversion method. This is a port of
   * <tt>burr1.c</tt> from the <A
   * HREF="http://www.cis.tu-graz.ac.at/stat/stadl/random.html">C-RAND /
   * WIN-RAND</A> library. C-RAND's implementation, in turn, is based upon
   * <p>
   * L. Devroye (1986): Non-Uniform Random Variate Generation, Springer
   * Verlag, New York.
   * <p>
   *
   * @param r
   *            must be &gt; 0.
   * @param nr
   *            the number of the burr distribution (e.g. 2,7,8,10).
   */
  def nextBurr1(r: Double, nr: Int, randomGenerator: DoubleRandomEngine): Double = {
    var y: Double = 0.0
    y = Math.exp(Math.log(randomGenerator.raw()) / r)
    nr match {
      case 2 => return (-Math.log(1 / y - 1))
      case 7 => return (Math.log(2 * y / (2 - 2 * y)) / 2)
      case 8 => return (Math.log(Math.tan(y * Math.PI / 2.0)))
      case 10 => return (Math.sqrt(-Math.log(1 - y)))
    }
    0
  }

  /**
   * Returns a random number from the Burr III, IV, V, VI, IX, XII
   * distributions.
   * <p>
   * <b>Implementation:</b> Inversion method. This is a port of
   * <tt>burr2.c</tt> from the <A
   * HREF="http://www.cis.tu-graz.ac.at/stat/stadl/random.html">C-RAND /
   * WIN-RAND</A> library. C-RAND's implementation, in turn, is based upon
   * <p>
   * L. Devroye (1986): Non-Uniform Random Variate Generation, Springer
   * Verlag, New York.
   * <p>
   *
   * @param r
   *            must be &gt; 0.
   * @param k
   *            must be &gt; 0.
   * @param nr
   *            the number of the burr distribution (e.g. 3,4,5,6,9,12).
   */
  def nextBurr2(r: Double, 
      k: Double, 
      nr: Int, 
      randomGenerator: DoubleRandomEngine): Double = {
    var y: Double = 0.0
    var u: Double = 0.0
    u = randomGenerator.raw()
    y = Math.exp(-Math.log(u) / r) - 1.0
    nr match {
      case 3 => return (Math.exp(-Math.log(y) / k))
      case 4 => 
        y = Math.exp(k * Math.log(y)) + 1.0
        y = k / y
        return (y)

      case 5 => 
        y = Math.atan(-Math.log(y / k))
        return (y)

      case 6 => 
        y = -Math.log(y / k) / r
        y = Math.log(y + Math.sqrt(y * y + 1.0))
        return (y)

      case 9 => 
        y = 1.0 + 2.0 * u / (k * (1.0 - u))
        y = Math.exp(Math.log(y) / r) - 1.0
        return Math.log(y)

      case 12 => return Math.exp(Math.log(y) / k)
    }
    0
  }

  /**
   * Returns a cauchy distributed random number from the standard Cauchy
   * distribution C(0,1). <A HREF=
   * "http://www.cern.ch/RD11/rkb/AN16pp/node25.html#SECTION000250000000000000000"
   * > math definition</A> and <A
   * HREF="http://www.statsoft.com/textbook/glosc.html#Cauchy Distribution">
   * animated definition</A>.
   * <p>
   * <tt>p(x) = 1/ (mean*pi * (1+(x/mean)^2))</tt>.
   * <p>
   * <b>Implementation:</b> This is a port of <tt>cin.c</tt> from the <A
   * HREF="http://www.cis.tu-graz.ac.at/stat/stadl/random.html">C-RAND /
   * WIN-RAND</A> library.
   * <p>
   *
   * @return a number in the open unit interval <code>(0.0,1.0)</code>
   *         (excluding 0.0 and 1.0).
   */
  def nextCauchy(randomGenerator: DoubleRandomEngine): Double = {
    Math.tan(Math.PI * randomGenerator.raw())
  }

  /**
   * Returns an erlang distributed random number with the given variance and
   * mean.
   */
  def nextErlang(variance: Double, mean: Double, randomGenerator: DoubleRandomEngine): Double = {
    var k = ((mean * mean) / variance + 0.5).toInt
    k = if ((k > 0)) k else 1
    val a = k / mean
    var prod = 1.0
    for (i <- 0 until k) prod *= randomGenerator.raw()
    -Math.log(prod) / a
  }

  /**
   * Returns a discrete geometric distributed random number; <A
   * HREF="http://www.statsoft.com/textbook/glosf.html#Geometric
   * Distribution">Definition</A>.
   * <p>
   * <tt>p(k) = p * (1-p)^k</tt> for <tt> k &gt;= 0</tt>.
   * <p>
   * <b>Implementation:</b> Inversion method. This is a port of <tt>geo.c</tt>
   * from the <A
   * HREF="http://www.cis.tu-graz.ac.at/stat/stadl/random.html">C-RAND /
   * WIN-RAND</A> library.
   *
   * @param p
   *            must satisfy <tt>0 &lt; p &lt; 1</tt>.
   *            <p>
   */
  def nextGeometric(p: Double, randomGenerator: DoubleRandomEngine): Int = {
    val u = randomGenerator.raw()
    (Math.log(u) / Math.log(1.0 - p)).toInt
  }

  /**
   * Returns a lambda distributed random number with parameters l3 and l4.
   * <p>
   * <b>Implementation:</b> Inversion method. This is a port of
   * <tt>lamin.c</tt> from the <A
   * HREF="http://www.cis.tu-graz.ac.at/stat/stadl/random.html">C-RAND /
   * WIN-RAND</A> library. C-RAND's implementation, in turn, is based upon
   * <p>
   * J.S. Ramberg, B:W. Schmeiser (1974): An approximate method for generating
   * asymmetric variables, Communications ACM 17, 78-82.
   * <p>
   */
  def nextLambda(l3: Double, l4: Double, randomGenerator: DoubleRandomEngine): Double = {
    var l_sign: Double = 0.0
    l_sign = if ((l3 < 0) || (l4 < 0)) -1.0 else 1.0
    val u = randomGenerator.raw()
    val x = l_sign * 
      (Math.exp(Math.log(u) * l3) - Math.exp(Math.log(1.0 - u) * l4))
    x
  }

  /**
   * Returns a Laplace (Double Exponential) distributed random number from the
   * standard Laplace distribution L(0,1).
   * <p>
   * <b>Implementation:</b> Inversion method. This is a port of
   * <tt>lapin.c</tt> from the <A
   * HREF="http://www.cis.tu-graz.ac.at/stat/stadl/random.html">C-RAND /
   * WIN-RAND</A> library.
   * <p>
   *
   * @return a number in the open unit interval <code>(0.0,1.0)</code>
   *         (excluding 0.0 and 1.0).
   */
  def nextLaplace(randomGenerator: DoubleRandomEngine): Double = {
    var u = randomGenerator.raw()
    u = u + u - 1.0
    if (u > 0) -Math.log(1.0 - u) else Math.log(1.0 + u)
  }

  /**
   * Returns a random number from the standard Logistic distribution Log(0,1).
   * <p>
   * <b>Implementation:</b> Inversion method. This is a port of
   * <tt>login.c</tt> from the <A
   * HREF="http://www.cis.tu-graz.ac.at/stat/stadl/random.html">C-RAND /
   * WIN-RAND</A> library.
   */
  def nextLogistic(randomGenerator: DoubleRandomEngine): Double = {
    val u = randomGenerator.raw()
    (-Math.log(1.0 / u - 1.0))
  }

  /**
   * Returns a power-law distributed random number with the given exponent and
   * lower cutoff.
   *
   * @param alpha
   *            the exponent
   * @param cut
   *            the lower cutoff
   */
  def nextPowLaw(alpha: Double, cut: Double, randomGenerator: DoubleRandomEngine): Double = {
    cut * 
      Math.pow(randomGenerator.raw(), 1.0 / (alpha + 1.0))
  }

  /**
   * Returns a random number from the standard Triangular distribution in
   * (-1,1).
   * <p>
   * <b>Implementation:</b> Inversion method. This is a port of <tt>tra.c</tt>
   * from the <A
   * HREF="http://www.cis.tu-graz.ac.at/stat/stadl/random.html">C-RAND /
   * WIN-RAND</A> library.
   * <p>
   */
  def nextTriangular(randomGenerator: DoubleRandomEngine): Double = {
    var u: Double = 0.0
    u = randomGenerator.raw()
    if (u <= 0.5) (Math.sqrt(2.0 * u) - 1.0) else (1.0 - Math.sqrt(2.0 * (1.0 - u)))
  }

  /**
   * Returns a weibull distributed random number. Polar method. See
   * Simulation, Modelling & Analysis by Law & Kelton, pp259
   */
  def nextWeibull(alpha: Double, beta: Double, randomGenerator: DoubleRandomEngine): Double = {
    Math.pow(beta * (-Math.log(1.0 - randomGenerator.raw())), 1.0 / alpha)
  }

  /**
   * Returns a zipfian distributed random number with the given skew.
   * <p>
   * Algorithm from page 551 of: Devroye, Luc (1986) `Non-uniform random
   * variate generation', Springer-Verlag: Berlin. ISBN 3-540-96305-7 (also
   * 0-387-96305-7)
   *
   * @param z
   *            the skew of the distribution (must be &gt;1.0).
   * @return a zipfian distributed number in the closed interval
   *         <tt>[1,Integer.MAX_VALUE]</tt>.
   */
  def nextZipfInt(z: Double, randomGenerator: DoubleRandomEngine): Int = {
    val b = Math.pow(2.0, z - 1.0)
    val constant = -1.0 / (z - 1.0)
    var result = 0
    while (true) {
      val u = randomGenerator.raw()
      val v = randomGenerator.raw()
      result = (Math.floor(Math.pow(u, constant))).toInt
      val t = Math.pow(1.0 + 1.0 / result, z - 1.0)
      if (v * result * (t - 1.0) / (b - 1.0) <= t / b) //break
    }
    result
  }
}

/**
 * Contains methods for conveniently generating pseudo-random numbers from
 * special distributions such as the Burr, Cauchy, Erlang, Geometric, Lambda,
 * Laplace, Logistic, Weibull, etc.
 * <p>
 * <b>About this class:</b>
 * <dt>All distributions are obtained by using a <b>uniform</b> pseudo-random
 * number generator. followed by a transformation to the desired distribution.
 * <p>
 * <b>Example usage:</b>
 *
 * <pre>
 * cern.jet.random.engine.RandomEngine generator;
 * generator = new cern.jet.random.engine.MersenneTwister(new java.util.Date());
 * //generator = new edu.cornell.lassp.houle.RngPack.Ranecu(new java.util.Date());
 * //generator = new edu.cornell.lassp.houle.RngPack.Ranmar(new java.util.Date());
 * //generator = new edu.cornell.lassp.houle.RngPack.Ranlux(new java.util.Date());
 * //generator = AbstractDistribution.makeDefaultGenerator();
 * for (int i=1000000; --i &gt;=0; ) {
 *    int cauchy = Distributions.nextCauchy(generator);
 *    ...
 * }
 * </pre>
 *
 * @see cern.jet.random.tdouble.engine.DoubleMersenneTwister
 * @see java.util.Random
 * @see java.lang.Math
 * @author wolfgang.hoschek@cern.ch
 * @version 1.0, 09/24/99
 */
class Distributions protected () {

  throw new RuntimeException("Non instantiable")
}
