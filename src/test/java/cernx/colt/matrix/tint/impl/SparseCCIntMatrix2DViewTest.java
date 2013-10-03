package cernx.colt.matrix.tint.impl;

public class SparseCCIntMatrix2DViewTest extends SparseCCIntMatrix2DTest {

    public SparseCCIntMatrix2DViewTest(String arg0) {
        super(arg0);
    }

    protected void createMatrices() throws Exception {
        A = new SparseCCIntMatrix2D(NCOLUMNS, NROWS).viewTranspose();
        B = new SparseCCIntMatrix2D(NCOLUMNS, NROWS).viewTranspose();
        Bt = new SparseCCIntMatrix2D(NROWS, NCOLUMNS).viewTranspose();
    }

}
