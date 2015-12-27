package cern.colt.matrix.tdouble.algo.solver;

public class DoubleGLSQRTest extends DoubleIterativeSolverTest {
    
    public DoubleGLSQRTest(String arg0) {
        super(arg0);
    }

    @Override
    protected void createSolver() throws Exception {
        solver = new DoubleGLSQR();
        M = solver.getPreconditioner(); //identity preconditioner
    }

}
