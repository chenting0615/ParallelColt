package cernx.colt.matrix.tfloat.impl;

public class DiagonalFloatMatrix2DViewTest extends DiagonalFloatMatrix2DTest {

    public DiagonalFloatMatrix2DViewTest(String arg0) {
        super(arg0);
    }

    protected void createMatrices() throws Exception {
        DINDEX = 3;
        A = new DiagonalFloatMatrix2D(NCOLUMNS, NROWS, -DINDEX);
        DLENGTH = ((DiagonalFloatMatrix2D) A).diagonalLength();
        A = A.viewTranspose();
        B = new DiagonalFloatMatrix2D(NCOLUMNS, NROWS, -DINDEX).viewTranspose();
        Bt = new DiagonalFloatMatrix2D(NROWS, NCOLUMNS, DINDEX).viewTranspose();
    }

}
