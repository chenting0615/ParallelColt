package cernx.colt.matrix.tlong.impl;

public class DiagonalLongMatrix2DViewTest extends DiagonalLongMatrix2DTest {

    public DiagonalLongMatrix2DViewTest(String arg0) {
        super(arg0);
    }

    protected void createMatrices() throws Exception {
        DINDEX = 3;
        A = new DiagonalLongMatrix2D(NCOLUMNS, NROWS, -DINDEX);
        DLENGTH = ((DiagonalLongMatrix2D) A).diagonalLength();
        A = A.viewTranspose();
        B = new DiagonalLongMatrix2D(NCOLUMNS, NROWS, -DINDEX).viewTranspose();
        Bt = new DiagonalLongMatrix2D(NROWS, NCOLUMNS, DINDEX).viewTranspose();
    }

}
