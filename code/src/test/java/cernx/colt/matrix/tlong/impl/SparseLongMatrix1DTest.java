package cernx.colt.matrix.tlong.impl;

import cernx.colt.matrix.tlong.LongMatrix1DTest;

public class SparseLongMatrix1DTest extends LongMatrix1DTest {

    public SparseLongMatrix1DTest(String arg0) {
        super(arg0);
    }

    protected void createMatrices() throws Exception {
        A = new SparseLongMatrix1D(SIZE);
        B = new SparseLongMatrix1D(SIZE);
    }
}
