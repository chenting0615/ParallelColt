package cernx.colt.matrix.tlong.impl;

public class DenseColumnLongMatrix2DViewTest extends DenseColumnLongMatrix2DTest {

    public DenseColumnLongMatrix2DViewTest(String arg0) {
        super(arg0);
    }

    protected void createMatrices() throws Exception {
        A = new DenseColumnLongMatrix2D(NCOLUMNS, NROWS).viewTranspose();
        B = new DenseColumnLongMatrix2D(NCOLUMNS, NROWS).viewTranspose();
        Bt = new DenseColumnLongMatrix2D(NROWS, NCOLUMNS).viewTranspose();
    }

}
