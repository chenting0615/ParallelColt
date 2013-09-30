package cernx.colt.matrix.tdouble;

import cernx.colt.matrix.tdouble.impl.DenseColumnDoubleMatrix2DTest;
import cernx.colt.matrix.tdouble.impl.DenseColumnDoubleMatrix2DViewTest;
import cernx.colt.matrix.tdouble.impl.DenseDoubleMatrix1DTest;
import cernx.colt.matrix.tdouble.impl.DenseDoubleMatrix1DViewTest;
import cernx.colt.matrix.tdouble.impl.DenseDoubleMatrix2DTest;
import cernx.colt.matrix.tdouble.impl.DenseDoubleMatrix2DViewTest;
import cernx.colt.matrix.tdouble.impl.DenseLargeDoubleMatrix2DTest;
import cernx.colt.matrix.tdouble.impl.DenseLargeDoubleMatrix2DViewTest;
import cernx.colt.matrix.tdouble.impl.DiagonalDoubleMatrix2DTest;
import cernx.colt.matrix.tdouble.impl.DiagonalDoubleMatrix2DViewTest;
import cernx.colt.matrix.tdouble.impl.SparseCCDoubleMatrix2DTest;
import cernx.colt.matrix.tdouble.impl.SparseCCDoubleMatrix2DViewTest;
import cernx.colt.matrix.tdouble.impl.SparseCCMDoubleMatrix2DTest;
import cernx.colt.matrix.tdouble.impl.SparseCCMDoubleMatrix2DViewTest;
import cernx.colt.matrix.tdouble.impl.SparseDoubleMatrix1DTest;
import cernx.colt.matrix.tdouble.impl.SparseDoubleMatrix1DViewTest;
import cernx.colt.matrix.tdouble.impl.SparseDoubleMatrix2DTest;
import cernx.colt.matrix.tdouble.impl.SparseDoubleMatrix2DViewTest;
import cernx.colt.matrix.tdouble.impl.SparseRCDoubleMatrix2DTest;
import cernx.colt.matrix.tdouble.impl.SparseRCDoubleMatrix2DViewTest;
import cernx.colt.matrix.tdouble.impl.SparseRCMDoubleMatrix2DTest;
import cernx.colt.matrix.tdouble.impl.SparseRCMDoubleMatrix2DViewTest;
import junit.framework.Test;
import junit.framework.TestSuite;

public class AllDoubleMatrixTests {

    public static Test suite() {
        TestSuite suite = new TestSuite("cern.colt.matrix.tdouble tests");
        suite.addTestSuite(DenseDoubleMatrix1DTest.class);
        suite.addTestSuite(DenseDoubleMatrix1DViewTest.class);
        suite.addTestSuite(SparseDoubleMatrix1DTest.class);
        suite.addTestSuite(SparseDoubleMatrix1DViewTest.class);

        suite.addTestSuite(DenseDoubleMatrix2DTest.class);
        suite.addTestSuite(DenseDoubleMatrix2DViewTest.class);
        suite.addTestSuite(DenseColumnDoubleMatrix2DTest.class);
        suite.addTestSuite(DenseColumnDoubleMatrix2DViewTest.class);
        suite.addTestSuite(DenseLargeDoubleMatrix2DTest.class);
        suite.addTestSuite(DenseLargeDoubleMatrix2DViewTest.class);

        suite.addTestSuite(SparseDoubleMatrix2DTest.class);
        suite.addTestSuite(SparseDoubleMatrix2DViewTest.class);
        suite.addTestSuite(DiagonalDoubleMatrix2DTest.class);
        suite.addTestSuite(DiagonalDoubleMatrix2DViewTest.class);

        suite.addTestSuite(SparseRCDoubleMatrix2DTest.class);
        suite.addTestSuite(SparseRCDoubleMatrix2DViewTest.class);
        suite.addTestSuite(SparseRCMDoubleMatrix2DTest.class);
        suite.addTestSuite(SparseRCMDoubleMatrix2DViewTest.class);

        suite.addTestSuite(SparseCCDoubleMatrix2DTest.class);
        suite.addTestSuite(SparseCCDoubleMatrix2DViewTest.class);
        suite.addTestSuite(SparseCCMDoubleMatrix2DTest.class);
        suite.addTestSuite(SparseCCMDoubleMatrix2DViewTest.class);

        return suite;
    }
}
