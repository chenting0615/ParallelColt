package cernx.colt.matrix.tfloat.impl;

public class DenseColumnFloatMatrix2DViewTest extends DenseColumnFloatMatrix2DTest {

    public DenseColumnFloatMatrix2DViewTest(String arg0) {
        super(arg0);
    }

    protected void createMatrices() throws Exception {
        A = new DenseColumnFloatMatrix2D(NCOLUMNS, NROWS).viewTranspose();
        B = new DenseColumnFloatMatrix2D(NCOLUMNS, NROWS).viewTranspose();
        Bt = new DenseColumnFloatMatrix2D(NROWS, NCOLUMNS).viewTranspose();
    }

}
