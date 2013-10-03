package cernx.colt.matrix.tfloat.impl;

public class SparseCCFloatMatrix2DViewTest extends SparseCCFloatMatrix2DTest {

    public SparseCCFloatMatrix2DViewTest(String arg0) {
        super(arg0);
    }

    protected void createMatrices() throws Exception {
        A = new SparseCCFloatMatrix2D(NCOLUMNS, NROWS).viewTranspose();
        B = new SparseCCFloatMatrix2D(NCOLUMNS, NROWS).viewTranspose();
        Bt = new SparseCCFloatMatrix2D(NROWS, NCOLUMNS).viewTranspose();
    }

}
