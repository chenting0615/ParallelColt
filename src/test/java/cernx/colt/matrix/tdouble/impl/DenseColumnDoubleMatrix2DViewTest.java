package cernx.colt.matrix.tdouble.impl;

public class DenseColumnDoubleMatrix2DViewTest extends DenseColumnDoubleMatrix2DTest {

    public DenseColumnDoubleMatrix2DViewTest(String arg0) {
        super(arg0);
    }

    protected void createMatrices() throws Exception {
        A = new DenseColumnDoubleMatrix2D(NCOLUMNS, NROWS).viewTranspose();
        B = new DenseColumnDoubleMatrix2D(NCOLUMNS, NROWS).viewTranspose();
        Bt = new DenseColumnDoubleMatrix2D(NROWS, NCOLUMNS).viewTranspose();
    }

}
