package cernx.colt.matrix.tfloat.impl;

public class DenseFloatMatrix2DViewTest extends DenseFloatMatrix2DTest {

    public DenseFloatMatrix2DViewTest(String arg0) {
        super(arg0);
    }

    protected void createMatrices() throws Exception {
        A = new DenseFloatMatrix2D(NCOLUMNS, NROWS).viewTranspose();
        B = new DenseFloatMatrix2D(NCOLUMNS, NROWS).viewTranspose();
        Bt = new DenseFloatMatrix2D(NROWS, NCOLUMNS).viewTranspose();
    }

}
