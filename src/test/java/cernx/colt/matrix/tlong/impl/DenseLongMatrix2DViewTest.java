package cernx.colt.matrix.tlong.impl;

public class DenseLongMatrix2DViewTest extends DenseLongMatrix2DTest {

    public DenseLongMatrix2DViewTest(String arg0) {
        super(arg0);
    }

    protected void createMatrices() throws Exception {
        A = new DenseLongMatrix2D(NCOLUMNS, NROWS).viewTranspose();
        B = new DenseLongMatrix2D(NCOLUMNS, NROWS).viewTranspose();
        Bt = new DenseLongMatrix2D(NROWS, NCOLUMNS).viewTranspose();
    }

}
