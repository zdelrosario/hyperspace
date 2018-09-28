### Pipe flow model

from numpy import array
from math import sqrt, log10
from scipy.optimize import bisect
from pyutil.numeric import grad as grad_fcn

# {rho,u,d,mu,eps}
dim_mat = [[ 1, 0, 0, 1, 0], # M
           [-3, 1, 1,-1, 1], # L
           [ 0,-1, 0,-1, 0]] # T

m = 5

# Parameter bound cases
Q_lo = []; Q_hi = []
Q_lo.append( array([1.0,1.0e-4,1.3,1.0e-5,1.0e-1]) ) # Laminar
Q_hi.append( array([1.4,1.0e-3,1.7,1.5e-5,1.5e-1]) )
Q_lo.append( array([1.0,0.4e-1,1.3,1.0e-5,1.0e-3]) ) # Transition
Q_hi.append( array([1.4,0.4e+0,1.7,1.5e-5,1.5e-3]) )
Q_lo.append( array([1.0,1.0e+0,1.3,1.0e-5,0.5e-1]) ) # Turbulent
Q_hi.append( array([1.4,1.0e+1,1.7,1.5e-5,2.0e-1]) )

w_u = [0,0,0,0,0]

def re_fcn(q):
    # {rho,u,d,mu,eps}
    return q[0]*q[1]*q[2]/q[3]

def f_lam(q):
    # {rho,u,d,mu,eps}
    return 64. / re_fcn(q)

def colebrook(q,f):
    # {rho,u,d,mu,eps}
    fs = sqrt(f); Re = re_fcn(q)
    return 1 + 2.*fs*log10(q[4]/3.6/q[2] + 2.51/Re/fs)

def f_tur(q):
    return bisect(lambda f: colebrook(q,f), 1e-5, 10)

Re_c = 3e3
def fcn(q):
    Re = re_fcn(q)
    if Re < Re_c:
        return f_lam(q)
    else:
        return f_tur(q)

def grad(q):
    return grad_fcn(q,fcn,h=1e-4)

if __name__ == "__main__":
    my_case = 0

    Q_nom = 0.5*(Q_lo[my_case]+Q_hi[my_case])
    Re_nom = re_fcn(Q_nom)
    Ep_nom = Q_nom[4]/Q_nom[2]

    f = fcn(Q_nom)
