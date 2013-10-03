package cernx.colt.matrix.tdouble.impl;

public class DenseDoubleMatrix2DViewTest extends DenseDoubleMatrix2DTest {

    public DenseDoubleMatrix2DViewTest(String arg0) {
        super(arg0);
    }

    protected void createMatrices() throws Exception {
        A = new DenseDoubleMatrix2D(NCOLUMNS, NROWS).viewTranspose();
        B = new DenseDoubleMatrix2D(NCOLUMNS, NROWS).viewTranspose();
        Bt = new DenseDoubleMatrix2D(NROWS, NCOLUMNS).viewTranspose();
    }

}
