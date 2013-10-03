package cernx.colt.matrix.tdouble.impl;

public class SparseRCMDoubleMatrix2DViewTest extends SparseRCMDoubleMatrix2DTest {

    public SparseRCMDoubleMatrix2DViewTest(String arg0) {
        super(arg0);
    }

    protected void createMatrices() throws Exception {
        A = new SparseRCMDoubleMatrix2D(NCOLUMNS, NROWS).viewTranspose();
        B = new SparseRCMDoubleMatrix2D(NCOLUMNS, NROWS).viewTranspose();
        Bt = new SparseRCMDoubleMatrix2D(NROWS, NCOLUMNS).viewTranspose();
    }

}
