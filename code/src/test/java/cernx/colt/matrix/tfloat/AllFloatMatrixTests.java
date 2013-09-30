package cernx.colt.matrix.tfloat;

import cernx.colt.matrix.tfloat.impl.DenseColumnFloatMatrix2DTest;
import cernx.colt.matrix.tfloat.impl.DenseColumnFloatMatrix2DViewTest;
import cernx.colt.matrix.tfloat.impl.DenseFloatMatrix1DViewTest;
import cernx.colt.matrix.tfloat.impl.DenseFloatMatrix2DTest;
import cernx.colt.matrix.tfloat.impl.DenseFloatMatrix2DViewTest;
import cernx.colt.matrix.tfloat.impl.DenseLargeFloatMatrix2DTest;
import cernx.colt.matrix.tfloat.impl.DenseLargeFloatMatrix2DViewTest;
import cernx.colt.matrix.tfloat.impl.DiagonalFloatMatrix2DTest;
import cernx.colt.matrix.tfloat.impl.SparseCCMFloatMatrix2DViewTest;
import cernx.colt.matrix.tfloat.impl.SparseFloatMatrix1DTest;
import cernx.colt.matrix.tfloat.impl.SparseFloatMatrix1DViewTest;
import cernx.colt.matrix.tfloat.impl.SparseFloatMatrix2DViewTest;
import cernx.colt.matrix.tfloat.impl.SparseRCFloatMatrix2DTest;
import cernx.colt.matrix.tfloat.impl.SparseRCFloatMatrix2DViewTest;
import cernx.colt.matrix.tfloat.impl.SparseRCMFloatMatrix2DTest;
import cernx.colt.matrix.tfloat.impl.SparseRCMFloatMatrix2DViewTest;
import junit.framework.Test;
import junit.framework.TestSuite;
import cern.colt.matrix.tfloat.algo.solver.AllFloatMatrixSolverTests;
import cernx.colt.matrix.tfloat.impl.DenseFloatMatrix1DTest;
import cern.colt.matrix.tfloat.impl.DenseFloatMatrix3DTest;
import cern.colt.matrix.tfloat.impl.DenseFloatMatrix3DViewTest;
import cern.colt.matrix.tfloat.impl.DenseLargeFloatMatrix3DTest;
import cern.colt.matrix.tfloat.impl.DenseLargeFloatMatrix3DViewTest;
import cernx.colt.matrix.tfloat.impl.DiagonalFloatMatrix2DViewTest;
import cernx.colt.matrix.tfloat.impl.SparseCCFloatMatrix2DTest;
import cernx.colt.matrix.tfloat.impl.SparseCCFloatMatrix2DViewTest;
import cernx.colt.matrix.tfloat.impl.SparseCCMFloatMatrix2DTest;
import cernx.colt.matrix.tfloat.impl.SparseFloatMatrix2DTest;
import cern.colt.matrix.tfloat.impl.SparseFloatMatrix3DTest;
import cern.colt.matrix.tfloat.impl.SparseFloatMatrix3DViewTest;

public class AllFloatMatrixTests {

    public static Test suite() {
        TestSuite suite = new TestSuite("cern.colt.matrix.tfloat tests");
        suite.addTestSuite(DenseFloatMatrix1DTest.class);
        suite.addTestSuite(DenseFloatMatrix1DViewTest.class);
        suite.addTestSuite(SparseFloatMatrix1DTest.class);
        suite.addTestSuite(SparseFloatMatrix1DViewTest.class);

        suite.addTestSuite(DenseFloatMatrix2DTest.class);
        suite.addTestSuite(DenseFloatMatrix2DViewTest.class);
        suite.addTestSuite(DenseColumnFloatMatrix2DTest.class);
        suite.addTestSuite(DenseColumnFloatMatrix2DViewTest.class);
        suite.addTestSuite(DenseLargeFloatMatrix2DTest.class);
        suite.addTestSuite(DenseLargeFloatMatrix2DViewTest.class);

        suite.addTestSuite(SparseFloatMatrix2DTest.class);
        suite.addTestSuite(SparseFloatMatrix2DViewTest.class);
        suite.addTestSuite(DiagonalFloatMatrix2DTest.class);
        suite.addTestSuite(DiagonalFloatMatrix2DViewTest.class);

        suite.addTestSuite(SparseRCFloatMatrix2DTest.class);
        suite.addTestSuite(SparseRCFloatMatrix2DViewTest.class);
        suite.addTestSuite(SparseRCMFloatMatrix2DTest.class);
        suite.addTestSuite(SparseRCMFloatMatrix2DViewTest.class);

        suite.addTestSuite(SparseCCFloatMatrix2DTest.class);
        suite.addTestSuite(SparseCCFloatMatrix2DViewTest.class);
        suite.addTestSuite(SparseCCMFloatMatrix2DTest.class);
        suite.addTestSuite(SparseCCMFloatMatrix2DViewTest.class);

        return suite;
    }
}
