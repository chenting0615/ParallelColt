package cern.colt

import junit.framework.Test
import junit.framework.TestSuite
//import cern.colt.matrix.tfloat.AllFloatMatrixTests
//import cern.colt.matrix.tlong.AllLongMatrixTests
//import cern.colt.matrix.tint.AllIntMatrixTests

//import cern.colt.matrix.tdcomplex.AllDComplexMatrixTests
//import cern.colt.matrix.tfcomplex.AllFComplexMatrixTests
//import cern.colt.matrix.tfloat.AllFloatMatrixTests
//import cern.colt.matrix.tint.AllIntMatrixTests
//import cern.colt.matrix.tlong.AllLongMatrixTests
import edu.emory.mathcs.utils.ConcurrencyUtils
import cern.colt.matrix.tdouble.impl.AllDoubleMatrixTests

object AllTests {

  def main(args: Array[String]) {
    org.junit.runner.JUnitCore.main(classOf[AllTests].getName)
  }
}

class AllTests {

  var NTHREADS: Int = 2

  def suite(): Test = {
    ConcurrencyUtils.setNumberOfThreads(NTHREADS)
    println("Running Parallel Colt tests using " + ConcurrencyUtils.getNumberOfThreads +
      " threads.")
    val suite = new TestSuite("Parallel Colt tests")
    suite.addTest(AllDoubleMatrixTests.suite())
    //suite.addTest(AllDComplexMatrixTests.suite())
    //suite.addTest(AllFloatMatrixTests.suite())
    //suite.addTest(AllFComplexMatrixTests.suite())
    //suite.addTest(AllLongMatrixTests.suite())
    //suite.addTest(AllIntMatrixTests.suite())
    suite
  }

}
