#ifndef LAPACKFLOW_H
#define LAPACKFLOW_H

#include "fortran.h"

SUBROUTINE_F77 solve_(INTEGER &N, INTEGER &NRHS, REAL* A, REAL *B, INTEGER &INFO);


inline void solve(int N, int NRHS, float *_A, float *_B)
{
   INTEGER INFO;
   FMATRIX<float> A(_A, N, N);
   FMATRIX<float> B(_B, N, NRHS);   
   solve_(N, NRHS, A, B, INFO);
}


#endif
