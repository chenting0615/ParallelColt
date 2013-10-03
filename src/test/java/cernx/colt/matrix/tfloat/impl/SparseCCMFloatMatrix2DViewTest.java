package cernx.colt.matrix.tfloat.impl;

public class SparseCCMFloatMatrix2DViewTest extends SparseCCMFloatMatrix2DTest {

    public SparseCCMFloatMatrix2DViewTest(String arg0) {
        super(arg0);
    }

    protected void createMatrices() throws Exception {
        A = new SparseCCMFloatMatrix2D(NCOLUMNS, NROWS).viewTranspose();
        B = new SparseCCMFloatMatrix2D(NCOLUMNS, NROWS).viewTranspose();
        Bt = new SparseCCMFloatMatrix2D(NROWS, NCOLUMNS).viewTranspose();
    }

}
