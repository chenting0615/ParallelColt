package cern.jet.random.tdouble.engine

object RandomSeedGenerator {

  /**
   * Prints the generated seeds for the given input parameters.
   */
  def main(args: Array[String]) {
    val row = Integer.parseInt(args(0))
    val column = Integer.parseInt(args(1))
    val size = Integer.parseInt(args(2))
    new RandomSeedGenerator(row, column).print(size)
  }
}

/**
 * Deterministic seed generator for pseudo-random number generators. The sole
 * purpose of this class is to decorrelate seeds and uniform random number
 * generators. (If a generator would be used to generate seeds for itself, the
 * result could be correlations.)
 * <p>
 * This class has entirelly deterministic behaviour: Constructing two instances
 * with the same parameters at any two distinct points in time will produce
 * identical seed sequences. However, it does not (at all) generate uniformly
 * distributed numbers. Do not use it as a uniformly distributed random number
 * generator!
 * <p>
 * Each generated sequence of seeds has a period of 10<sup>9</sup> numbers.
 * Internally uses {@link RandomSeedTable}.
 *
 * @author wolfgang.hoschek@cern.ch
 * @version 1.0, 09/24/99
 */
@SerialVersionUID(1L)
class RandomSeedGenerator(protected var row: Int, protected var column: Int)
    extends cern.colt.PersistentObject {

  /**
   * Constructs and returns a new seed generator.
   */
  def this() {
    this(0, 0)
  }

  /**
   * Returns the next seed.
   */
  def nextSeed(): Int = {
    val rc = RandomSeedTable.getSeedAtRowColumn(row, column)
    row += 1
    rc
  }

  /**
   * Prints the next <tt>size</tt> generated seeds.
   */
  def print(size: Int) {
    println("Generating " + size + " random seeds...")
    val copy = this.clone().asInstanceOf[RandomSeedGenerator]
    for (i <- 0 until size) {
      val seed = copy.nextSeed()
      println(seed)
    }
    println("\ndone.")
  }
}
