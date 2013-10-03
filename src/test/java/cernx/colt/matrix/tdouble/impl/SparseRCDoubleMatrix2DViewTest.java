package cernx.colt.matrix.tdouble.impl;

public class SparseRCDoubleMatrix2DViewTest extends SparseRCDoubleMatrix2DTest {

    public SparseRCDoubleMatrix2DViewTest(String arg0) {
        super(arg0);
    }

    protected void createMatrices() throws Exception {
        A = new SparseRCDoubleMatrix2D(NCOLUMNS, NROWS).viewTranspose();
        B = new SparseRCDoubleMatrix2D(NCOLUMNS, NROWS).viewTranspose();
        Bt = new SparseRCDoubleMatrix2D(NROWS, NCOLUMNS).viewTranspose();
    }

}
