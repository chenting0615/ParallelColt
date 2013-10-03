package cernx.colt.matrix.tlong.impl;

public class DenseLargeLongMatrix2DViewTest extends DenseLargeLongMatrix2DTest {

    public DenseLargeLongMatrix2DViewTest(String arg0) {
        super(arg0);
    }

    protected void createMatrices() throws Exception {
        A = new DenseLargeLongMatrix2D(NCOLUMNS, NROWS).viewTranspose();
        B = new DenseLargeLongMatrix2D(NCOLUMNS, NROWS).viewTranspose();
        Bt = new DenseLargeLongMatrix2D(NROWS, NCOLUMNS).viewTranspose();
    }
}
