package cern.jet.random.tdouble

object CliTestNormal {

  def main(argv: Array[String]) {
    val normal = new Normal(0.0, 1.0, Normal.makeDefaultGenerator())
    val x1 = normal.nextDouble(0, 0)
    val x2 = normal.nextDouble(0, 1)
    println(x1)
    println(x2)
  }
}
