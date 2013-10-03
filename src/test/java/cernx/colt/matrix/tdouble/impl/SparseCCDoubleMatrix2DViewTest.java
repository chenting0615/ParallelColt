package cernx.colt.matrix.tdouble.impl;

public class SparseCCDoubleMatrix2DViewTest extends SparseCCDoubleMatrix2DTest {

    public SparseCCDoubleMatrix2DViewTest(String arg0) {
        super(arg0);
    }

    protected void createMatrices() throws Exception {
        A = new SparseCCDoubleMatrix2D(NCOLUMNS, NROWS).viewTranspose();
        B = new SparseCCDoubleMatrix2D(NCOLUMNS, NROWS).viewTranspose();
        Bt = new SparseCCDoubleMatrix2D(NROWS, NCOLUMNS).viewTranspose();
    }

}
