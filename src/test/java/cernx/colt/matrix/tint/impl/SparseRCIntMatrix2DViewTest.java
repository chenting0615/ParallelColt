package cernx.colt.matrix.tint.impl;

public class SparseRCIntMatrix2DViewTest extends SparseRCIntMatrix2DTest {

    public SparseRCIntMatrix2DViewTest(String arg0) {
        super(arg0);
    }

    protected void createMatrices() throws Exception {
        A = new SparseRCIntMatrix2D(NCOLUMNS, NROWS).viewTranspose();
        B = new SparseRCIntMatrix2D(NCOLUMNS, NROWS).viewTranspose();
        Bt = new SparseRCIntMatrix2D(NROWS, NCOLUMNS).viewTranspose();
    }

}
