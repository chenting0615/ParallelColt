package cernx.colt.matrix.tfloat.impl;

public class SparseFloatMatrix2DViewTest extends SparseFloatMatrix2DTest {

    public SparseFloatMatrix2DViewTest(String arg0) {
        super(arg0);
    }

    protected void createMatrices() throws Exception {
        A = new SparseFloatMatrix2D(NCOLUMNS, NROWS).viewTranspose();
        B = new SparseFloatMatrix2D(NCOLUMNS, NROWS).viewTranspose();
        Bt = new SparseFloatMatrix2D(NROWS, NCOLUMNS).viewTranspose();
    }
}
