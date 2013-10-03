package cernx.colt.matrix.tfloat.impl;

public class SparseRCFloatMatrix2DViewTest extends SparseRCFloatMatrix2DTest {

    public SparseRCFloatMatrix2DViewTest(String arg0) {
        super(arg0);
    }

    protected void createMatrices() throws Exception {
        A = new SparseRCFloatMatrix2D(NCOLUMNS, NROWS).viewTranspose();
        B = new SparseRCFloatMatrix2D(NCOLUMNS, NROWS).viewTranspose();
        Bt = new SparseRCFloatMatrix2D(NROWS, NCOLUMNS).viewTranspose();
    }

}
