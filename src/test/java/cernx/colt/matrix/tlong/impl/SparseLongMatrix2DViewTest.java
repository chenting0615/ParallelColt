package cernx.colt.matrix.tlong.impl;

public class SparseLongMatrix2DViewTest extends SparseLongMatrix2DTest {

    public SparseLongMatrix2DViewTest(String arg0) {
        super(arg0);
    }

    protected void createMatrices() throws Exception {
        A = new SparseLongMatrix2D(NCOLUMNS, NROWS).viewTranspose();
        B = new SparseLongMatrix2D(NCOLUMNS, NROWS).viewTranspose();
        Bt = new SparseLongMatrix2D(NROWS, NCOLUMNS).viewTranspose();
    }
}
