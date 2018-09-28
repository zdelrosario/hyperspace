## Generate data for dimension reduction examples
# --------------------------------------------------
import numpy as np
import pyutil.numeric as ut
import pandas as pd

from scipy.linalg import svd
from m_pipe import dim_mat, fcn, grad, Q_lo, Q_hi, m

np.random.seed(101)

## Script parameters
bnd_idx = 2 # 0: laminar, 1: transition, 2: turbulent
N = int(1e3)

Q = np.random.random((N, m)) * (Q_hi[bnd_idx] - Q_lo[bnd_idx]) + Q_lo[bnd_idx]

## Run computations
# --------------------------------------------------
## Active subspace
F = np.array([fcn(q) for q in Q])
G = np.array([grad(q) for q in Q])
C = np.dot(G.T, G)
[U, s, Vh] = svd(C)

## Active subspace; pi trick
Gpi = np.array([Q[ind] * G[ind] for ind in range(N)])
Cpi = np.dot(Gpi.T, Gpi)
[Upi, spi, Vhpi] = svd(Cpi)

## Report
# --------------------------------------------------
print("s           = \n{}".format(s))
print("spi         = \n{}".format(spi))

## Write out
# --------------------------------------------------
d = {
    'N': N,
    'bnd_idx': bnd_idx,
    'F': F,
    'G1': G[:, 0],
    'G2': G[:, 1],
    'G3': G[:, 2],
    'G4': G[:, 3],
    'G5': G[:, 4],
    'Q1': Q[:, 0],
    'Q2': Q[:, 1],
    'Q3': Q[:, 2],
    'Q4': Q[:, 3],
    'Q5': Q[:, 4]
}

df = pd.DataFrame(data = d)
df.to_csv(path_or_buf = "as_data{}.csv".format(bnd_idx))
