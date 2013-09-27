package cernx.colt.matrix.tlong.impl;

import cernx.colt.matrix.tlong.LongMatrix2DTest;

public class SparseCCMLongMatrix2DTest extends LongMatrix2DTest {

    public SparseCCMLongMatrix2DTest(String arg0) {
        super(arg0);
    }

    protected void createMatrices() throws Exception {
        A = new SparseCCMLongMatrix2D(NROWS, NCOLUMNS);
        B = new SparseCCMLongMatrix2D(NROWS, NCOLUMNS);
        Bt = new SparseCCMLongMatrix2D(NCOLUMNS, NROWS);
    }

}
