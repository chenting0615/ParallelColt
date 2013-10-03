package cernx.colt.matrix.tfloat.impl;

public class SparseRCMFloatMatrix2DViewTest extends SparseRCMFloatMatrix2DTest {

    public SparseRCMFloatMatrix2DViewTest(String arg0) {
        super(arg0);
    }

    protected void createMatrices() throws Exception {
        A = new SparseRCMFloatMatrix2D(NCOLUMNS, NROWS).viewTranspose();
        B = new SparseRCMFloatMatrix2D(NCOLUMNS, NROWS).viewTranspose();
        Bt = new SparseRCMFloatMatrix2D(NROWS, NCOLUMNS).viewTranspose();
    }

}
