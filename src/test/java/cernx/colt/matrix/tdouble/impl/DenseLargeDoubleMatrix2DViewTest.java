package cernx.colt.matrix.tdouble.impl;

public class DenseLargeDoubleMatrix2DViewTest extends DenseLargeDoubleMatrix2DTest {

    public DenseLargeDoubleMatrix2DViewTest(String arg0) {
        super(arg0);
    }

    protected void createMatrices() throws Exception {
        A = new DenseLargeDoubleMatrix2D(NCOLUMNS, NROWS).viewTranspose();
        B = new DenseLargeDoubleMatrix2D(NCOLUMNS, NROWS).viewTranspose();
        Bt = new DenseLargeDoubleMatrix2D(NROWS, NCOLUMNS).viewTranspose();
    }
}
