package cernx.colt.matrix.tint.impl;

public class DenseIntMatrix2DViewTest extends DenseIntMatrix2DTest {

    public DenseIntMatrix2DViewTest(String arg0) {
        super(arg0);
    }

    protected void createMatrices() throws Exception {
        A = new DenseIntMatrix2D(NCOLUMNS, NROWS).viewTranspose();
        B = new DenseIntMatrix2D(NCOLUMNS, NROWS).viewTranspose();
        Bt = new DenseIntMatrix2D(NROWS, NCOLUMNS).viewTranspose();
    }

}
