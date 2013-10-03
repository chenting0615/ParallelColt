package cernx.colt.matrix.tdouble.impl;

public class SparseDoubleMatrix2DViewTest extends SparseDoubleMatrix2DTest {

    public SparseDoubleMatrix2DViewTest(String arg0) {
        super(arg0);
    }

    protected void createMatrices() throws Exception {
        A = new SparseDoubleMatrix2D(NCOLUMNS, NROWS).viewTranspose();
        B = new SparseDoubleMatrix2D(NCOLUMNS, NROWS).viewTranspose();
        Bt = new SparseDoubleMatrix2D(NROWS, NCOLUMNS).viewTranspose();
    }
}
