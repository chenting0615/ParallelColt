package cern.colt.matrix.tdouble.algo.solver;

import cern.colt.matrix.tdouble.DoubleMatrix1D;
import cern.colt.matrix.tdouble.DoubleMatrix2D;
import cern.colt.matrix.tdouble.algo.DenseDoubleAlgebra;
import cern.jet.math.tdouble.DoubleFunctions;

/**
 * GLSQR is the generalised LSQR method implemented after
 * <a href="http://www.math.kent.edu/~reichel/publications/glsqr.pdf">this paper</a>.
 * The method is used to solve the problem Ax = b where A is large and the problem
 * is ill-posed.
 * @author Severin Wischmann (severin.wischmann (at) gmail.com)
 */
public class DoubleGLSQR extends AbstractDoubleIterativeSolver {
    private static final DenseDoubleAlgebra alg = DenseDoubleAlgebra.DEFAULT;
    private static final double sqrteps = Math.sqrt(Math.pow(2, -52));
    private double phi_hat;
    /**
     * Default constructor.
     */
    public DoubleGLSQR() {
        iter = new DefaultDoubleIterationMonitor();
        ((DefaultDoubleIterationMonitor) iter).setRelativeTolerance(-1.);
    }
    /**
     * Sets the relative tolerance of the algorithm.
     * @param relTol The relative tolerance to be used.
     */
    public DoubleGLSQR(final double relTol) {
        iter = new DefaultDoubleIterationMonitor();
        ((DefaultDoubleIterationMonitor) iter).setRelativeTolerance(relTol);
    }

    @Override
    public DoubleMatrix1D solve(final DoubleMatrix2D A, final DoubleMatrix1D b, final DoubleMatrix1D v1)
            throws IterativeSolverDoubleNotConvergedException {
        assert(!Double.isNaN(v1.get(0))) : "Vector is NaN";
        checkSizes(A, b, v1);
        double psi, phi, rho_hat, rho0, rho_1, rho_2,
                c0, c_1, c_2, s0, s_1, s_2,
                sigma_1, sigma0, sigma1, tau_1, tau0, tau1, gamma;
        rho0 = alg.norm2(v1);
        if(rho0 != 0) {
            v1.assign(DoubleFunctions.div(alg.norm2(v1)));
        } else {
            v1.assign(1.0).assign(DoubleFunctions.div(alg.norm2(v1)));
        }
        final DoubleMatrix1D u1, u0, u_1, v0, v_1, w0, w_1, w_2, x0, x_1, z, z1;
        final double eps = Math.nextAfter(1.0, Double.POSITIVE_INFINITY) - 1.0;
        int d = 0;
        
        phi_hat = alg.norm2(b);
        u1 = b.copy();
        u1.assign(DoubleFunctions.div(phi_hat));
        u0 = b.copy();
        u0.assign(0.);
        u_1 = b.copy();
        v_1 = v1.copy();
        v0 = v1.copy();
        v0.assign(0.);
        w_2 = v1.copy();
        w_1 = v1.copy();
        w_1.assign(0.);
        w0 = v1.copy();
        w0.assign(0.);
        x_1 = v1.copy();
        x0 = v1.copy();
        x0.assign(0.);
        z = v1.copy();
        z1 = b.copy();
        
        if (((DefaultDoubleIterationMonitor) iter).getRelativeTolerance() == -1.0) {
            ((DefaultDoubleIterationMonitor) iter).setRelativeTolerance(sqrteps * alg.norm2(A.zMult(b, null, 1, 0, true)));
        }
        
        rho0 = rho_1 =  1;
        c_1  = c0    = -1;
        s_1  = s0    =  0;

        for(iter.setFirst(); !iter.converged(phi_hat); iter.next()) {
            // time shift
            u_1.assign(u0); u0.assign(u1);
            v_1.assign(v0); v0.assign(v1);
            c_2 = c_1; c_1 = c0;
            s_2 = s_1; s_1 = s0;
            x_1.assign(x0);
            w_2.assign(w_1); w_1.assign(w0);
            rho_2 = rho_1; rho_1 = rho0;
            // Lanczos bi-tridiagonalisation
            A.zMult(u0, z, 1, 0, true);
            sigma_1 = v_1.zDotProduct(z);
            z.assign(v_1, DoubleFunctions.minusMult(sigma_1));
            if(d == 0) {
                sigma0 = v0.zDotProduct(z);
                z.assign(v0, DoubleFunctions.minusMult(sigma0));
                sigma1 = alg.norm2(z);
                if(sigma1 > eps) {
                    v1.assign(z).assign(DoubleFunctions.div(sigma1));
                } else {
                    d = 1;
                }
            } else {
                sigma0 = alg.norm2(z);
                if(sigma0 > eps) {
                    v0.assign(z).assign(DoubleFunctions.div(sigma0));
                } else {
                    break;
                }
            }
            A.zMult(v0, z1);
            tau_1 = u_1.zDotProduct(z1);
            z1.assign(u_1, DoubleFunctions.minusMult(tau_1));
            tau0 = u0.zDotProduct(z1);
            z1.assign(u0, DoubleFunctions.minusMult(tau0));
            tau1 = alg.norm2(z1);
            if(tau1 > eps) {
                u1.assign(z1).assign(DoubleFunctions.div(tau1));
            }
            // solution update
            gamma   = s_2 * tau_1;
            psi     = -c_1 * c_2 * tau_1 + s_1 * tau0;
            rho_hat = -s_1 * c_2 * tau_1 - c_1 * tau0;
            rho0    = Math.sqrt(rho_hat * rho_hat + tau1 * tau1);
            c0      = rho_hat / rho0;
            s0      = tau1 / rho0;
            phi     = c0 * phi_hat;
            phi_hat = s0 * phi_hat;
            w0.assign(v0).assign(w_1, DoubleFunctions.minusMult(psi / rho_1))
                .assign(w_2, DoubleFunctions.minusMult(gamma / rho_2));
            x0.assign(x_1).assign(w0, DoubleFunctions.plusMultSecond(phi / rho0));
            if(tau1 <= eps) {
                break;
            }
        }
        v1.assign(x0);
        return v1;
    }
    /**
     * Returns the residual norm of the algorithm.
     * @return
     */
    public double getResidualNorm() {
        return Math.abs(phi_hat);
    }
    @Override
    protected void checkSizes(final DoubleMatrix2D A, final DoubleMatrix1D b, final DoubleMatrix1D x) {
        if (A.columns() != x.size())
            throw new IllegalArgumentException("A.columns() != x.size()");
        if (b.size() != A.rows())
            throw new IllegalArgumentException("b.size() != A.rows()");
    }
}
