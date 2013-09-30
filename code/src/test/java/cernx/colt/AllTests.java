package cernx.colt;

import cernx.colt.matrix.tint.AllIntMatrixTests;
import junit.framework.Test;
import junit.framework.TestSuite;
import cernx.colt.matrix.tdouble.AllDoubleMatrixTests;
import cernx.colt.matrix.tfloat.AllFloatMatrixTests;
import cernx.colt.matrix.tlong.AllLongMatrixTests;
import edu.emory.mathcs.utils.ConcurrencyUtils;

public class AllTests {

    public static int NTHREADS = 2;

    public static Test suite() {
        ConcurrencyUtils.setNumberOfThreads(NTHREADS);
        System.out.println("Running Parallel Colt tests using " + ConcurrencyUtils.getNumberOfThreads() + " threads.");
        TestSuite suite = new TestSuite("Parallel Colt tests");
        suite.addTest(AllDoubleMatrixTests.suite());
        suite.addTest(AllFloatMatrixTests.suite());
        suite.addTest(AllLongMatrixTests.suite());
        suite.addTest(AllIntMatrixTests.suite());
        return suite;
    }

    public static void main(String[] args) {
        org.junit.runner.JUnitCore.main(AllTests.class.getName().toString());
    }
}
