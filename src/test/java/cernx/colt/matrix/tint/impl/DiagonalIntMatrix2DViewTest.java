package cernx.colt.matrix.tint.impl;

public class DiagonalIntMatrix2DViewTest extends DiagonalIntMatrix2DTest {

    public DiagonalIntMatrix2DViewTest(String arg0) {
        super(arg0);
    }

    protected void createMatrices() throws Exception {
        DINDEX = 3;
        A = new DiagonalIntMatrix2D(NCOLUMNS, NROWS, -DINDEX);
        DLENGTH = ((DiagonalIntMatrix2D) A).diagonalLength();
        A = A.viewTranspose();
        B = new DiagonalIntMatrix2D(NCOLUMNS, NROWS, -DINDEX).viewTranspose();
        Bt = new DiagonalIntMatrix2D(NROWS, NCOLUMNS, DINDEX).viewTranspose();
    }

}
