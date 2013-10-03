package cernx.colt.matrix.tlong.impl;

public class SparseCCLongMatrix2DViewTest extends SparseCCLongMatrix2DTest {

    public SparseCCLongMatrix2DViewTest(String arg0) {
        super(arg0);
    }

    protected void createMatrices() throws Exception {
        A = new SparseCCLongMatrix2D(NCOLUMNS, NROWS).viewTranspose();
        B = new SparseCCLongMatrix2D(NCOLUMNS, NROWS).viewTranspose();
        Bt = new SparseCCLongMatrix2D(NROWS, NCOLUMNS).viewTranspose();
    }

}
