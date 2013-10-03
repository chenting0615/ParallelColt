package cernx.colt.matrix.tint.impl;

public class SparseCCMIntMatrix2DViewTest extends SparseCCMIntMatrix2DTest {

    public SparseCCMIntMatrix2DViewTest(String arg0) {
        super(arg0);
    }

    protected void createMatrices() throws Exception {
        A = new SparseCCMIntMatrix2D(NCOLUMNS, NROWS).viewTranspose();
        B = new SparseCCMIntMatrix2D(NCOLUMNS, NROWS).viewTranspose();
        Bt = new SparseCCMIntMatrix2D(NROWS, NCOLUMNS).viewTranspose();
    }

}
