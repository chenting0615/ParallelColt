package cernx.colt.matrix.tlong.impl;

public class SparseRCMLongMatrix2DViewTest extends SparseRCMLongMatrix2DTest {

    public SparseRCMLongMatrix2DViewTest(String arg0) {
        super(arg0);
    }

    protected void createMatrices() throws Exception {
        A = new SparseRCMLongMatrix2D(NCOLUMNS, NROWS).viewTranspose();
        B = new SparseRCMLongMatrix2D(NCOLUMNS, NROWS).viewTranspose();
        Bt = new SparseRCMLongMatrix2D(NROWS, NCOLUMNS).viewTranspose();
    }

}
