package cernx.colt.matrix.tdouble.impl;

public class SparseCCMDoubleMatrix2DViewTest extends SparseCCMDoubleMatrix2DTest {

    public SparseCCMDoubleMatrix2DViewTest(String arg0) {
        super(arg0);
    }

    protected void createMatrices() throws Exception {
        A = new SparseCCMDoubleMatrix2D(NCOLUMNS, NROWS).viewTranspose();
        B = new SparseCCMDoubleMatrix2D(NCOLUMNS, NROWS).viewTranspose();
        Bt = new SparseCCMDoubleMatrix2D(NROWS, NCOLUMNS).viewTranspose();
    }

}
