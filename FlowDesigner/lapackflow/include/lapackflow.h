#ifndef LAPACKFLOW_H
#define LAPACKFLOW_H

#include "fortran.h"
#include <iostream>

SUBROUTINE_F77 solve_(INTEGER &N, INTEGER &NRHS, REAL* A, REAL *B, INTEGER &INFO);

SUBROUTINE_F77 eig_(INTEGER &N, REAL* A, REAL *D, REAL* V);

inline void solve(int N, int NRHS, float *_A, float *_B);
inline void eig(int N, float *_A, float *D, float *_V);
#endif
