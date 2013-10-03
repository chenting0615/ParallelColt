package cernx.colt.matrix.tint.impl;

public class DenseColumnIntMatrix2DViewTest extends DenseColumnIntMatrix2DTest {

    public DenseColumnIntMatrix2DViewTest(String arg0) {
        super(arg0);
    }

    protected void createMatrices() throws Exception {
        A = new DenseColumnIntMatrix2D(NCOLUMNS, NROWS).viewTranspose();
        B = new DenseColumnIntMatrix2D(NCOLUMNS, NROWS).viewTranspose();
        Bt = new DenseColumnIntMatrix2D(NROWS, NCOLUMNS).viewTranspose();
    }

}
