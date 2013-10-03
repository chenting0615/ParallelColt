package cernx.colt.matrix.tfloat.impl;

public class DenseLargeFloatMatrix2DViewTest extends DenseLargeFloatMatrix2DTest {

    public DenseLargeFloatMatrix2DViewTest(String arg0) {
        super(arg0);
    }

    protected void createMatrices() throws Exception {
        A = new DenseLargeFloatMatrix2D(NCOLUMNS, NROWS).viewTranspose();
        B = new DenseLargeFloatMatrix2D(NCOLUMNS, NROWS).viewTranspose();
        Bt = new DenseLargeFloatMatrix2D(NROWS, NCOLUMNS).viewTranspose();
    }
}
