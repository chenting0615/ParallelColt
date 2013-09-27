package cern.jet.random.tdouble

import cern.jet.random.tdouble.engine.DoubleRandomEngine

object AbstractDoubleDistribution {

  /**
   * Constructs and returns a new uniform random number generation engine
   * seeded with the current time. Currently this is
   * {@link cern.jet.random.tdouble.engine.DoubleMersenneTwister}.
   */
  def makeDefaultGenerator(): DoubleRandomEngine = {
    cern.jet.random.tdouble.engine.DoubleRandomEngine.makeDefault()
  }
}

/**
 * Abstract base class for all random distributions.
 *
 * A subclass of this class need to override method <tt>nextDouble()</tt> and,
 * in rare cases, also <tt>nextInt()</tt>.
 * <p>
 * Currently all subclasses use a uniform pseudo-random number generation engine
 * and transform its results to the target distribution. Thus, they expect such
 * a uniform engine upon instance construction.
 * <p>
 * {@link cern.jet.random.tdouble.engine.DoubleMersenneTwister} is recommended
 * as uniform pseudo-random number generation engine, since it is very strong
 * and at the same time quick. {@link #makeDefaultGenerator()} will conveniently
 * construct and return such a magic thing. You can also, for example, use
 * {@link cern.jet.random.tdouble.engine.DRand}, a quicker (but much weaker)
 * uniform random number generation engine. Of course, you can also use other
 * strong uniform random number generation engines.
 *
 * <p>
 * <b>Ressources on the Web:</b>
 * <dt>Check the Web version of the <A
 * HREF="http://www.cern.ch/RD11/rkb/AN16pp/node1.html"> CERN Data Analysis
 * Briefbook </A>. This will clarify the definitions of most distributions.
 * <dt>Also consult the <A
 * HREF="http://www.statsoftinc.com/textbook/stathome.html"> StatSoft Electronic
 * Textbook</A> - the definite web book.
 * <p>
 * <b>Other useful ressources:</b>
 * <dt><A HREF=
 * "http://www.stats.gla.ac.uk/steps/glossary/probability_distributions.html">
 * Another site </A> and <A
 * HREF="http://www.statlets.com/usermanual/glossary.htm"> yet another site
 * </A>describing the definitions of several distributions.
 * <dt>You may want to check out a <A
 * HREF="http://www.stat.berkeley.edu/users/stark/SticiGui/Text/gloss.htm">
 * Glossary of Statistical Terms</A>.
 * <dt>The GNU Scientific Library contains an extensive (but hardly readable) <A
 * HREF="http://sourceware.cygnus.com/gsl/html/gsl-ref_toc.html#TOC26"> list of
 * definition of distributions</A>.
 * <dt>Use this Web interface to <A
 * HREF="http://www.stat.ucla.edu/calculators/cdf"> plot all sort of
 * distributions</A>.
 * <dt>Even more ressources: <A
 * HREF="http://www.animatedsoftware.com/statglos/statglos.htm"> Internet
 * glossary of Statistical Terms</A>, <A
 * HREF="http://www.ruf.rice.edu/~lane/hyperstat/index.html"> a text book</A>,
 * <A HREF="http://www.stat.umn.edu/~jkuhn/courses/stat3091f/stat3091f.html">
 * another text book</A>.
 * <dt>Finally, a good link list <A
 * HREF="http://www.execpc.com/~helberg/statistics.html"> Statistics on the
 * Web</A>.
 * <p>
 *
 * @see cern.jet.random.tdouble.engine
 * @author wolfgang.hoschek@cern.ch
 * @version 1.0, 09/24/99
 */
@SerialVersionUID(1L)
abstract class AbstractDoubleDistribution protected () extends cern.colt.PersistentObject {

  protected var randomGenerator: DoubleRandomEngine = _

  /**
   * Returns a deep copy of the receiver; the copy will produce identical
   * sequences. After this call has returned, the copy and the receiver have
   * equal but separate state.
   *
   * @return a copy of the receiver.
   */
  override def clone(): AnyRef = {
    val copy = super.clone().asInstanceOf[AbstractDoubleDistribution]
    if (this.randomGenerator != null) copy.randomGenerator = this.randomGenerator.clone().asInstanceOf[DoubleRandomEngine]
    copy
  }

  /**
   * Returns the used uniform random number generator;
   */
  protected def getRandomGenerator: DoubleRandomEngine = randomGenerator

  /**
   * Returns a random number from the distribution.
   */
  def nextDouble(): Double

  /**
   * Returns a random number from the distribution; returns
   * <tt>(int) Math.round(nextDouble())</tt>. Override this method if
   * necessary.
   */
  def nextInt(): Int = Math.round(nextDouble()).toInt

  /**
   * Sets the uniform random generator internally used.
   */
  protected def setRandomGenerator(randomGenerator: DoubleRandomEngine) {
    this.randomGenerator = randomGenerator
  }
}
