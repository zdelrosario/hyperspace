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
N_all = np.array([1e3, 1e4, 1e5], dtype = np.integer)

## Run computations
# --------------------------------------------------
S_all = np.zeros((len(N_all), m))

## Sobol' indices
for idx in range(len(N_all)):
    Q1 = np.random.random((N_all[idx], m)) * \
         (Q_hi[bnd_idx] - Q_lo[bnd_idx]) + Q_lo[bnd_idx]
    Q2 = np.random.random((N_all[idx], m)) * \
         (Q_hi[bnd_idx] - Q_lo[bnd_idx]) + Q_lo[bnd_idx]

    S_all[idx, :] = ut.dr_sobol(fcn, Q1, Q2)

## Report
# --------------------------------------------------
print("Sobol_total = \n{}".format(S_all))

## Write out
# --------------------------------------------------
d = {
    "N": N_all,
    "S_T1": S_all[:, 0],
    "S_T2": S_all[:, 1],
    "S_T3": S_all[:, 2],
    "S_T4": S_all[:, 3],
    "S_T5": S_all[:, 4]
}

df = pd.DataFrame(data = d)
df.to_csv(path_or_buf = "sobol_data{}.csv".format(bnd_idx))
