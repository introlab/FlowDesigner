#include "fortran.h"
#include <iostream>

SUBROUTINE_F77 solve_(INTEGER &N, INTEGER &NRHS, REAL* A, REAL *B, INTEGER &INFO);

SUBROUTINE SGETRS(CHARACTER &TRANS, INTEGER &N, INTEGER &NRHS, REAL* A, INTEGER &LDA, 
		  INTEGER *IPIV, REAL *B, INTEGER &LDB, INTEGER &INFO);
SUBROUTINE_F77 sgetrs_(CHARACTER &TRANS, INTEGER &N, INTEGER &NRHS, REAL* A, INTEGER &LDA, 
		  INTEGER *IPIV, REAL *B, INTEGER &LDB, INTEGER &INFO);
SUBROUTINE_F77 sgetrs2_(CHARACTER TRANS, INTEGER &N, INTEGER &NRHS, REAL* A, INTEGER &LDA, 
		  INTEGER *IPIV, REAL *B, INTEGER &LDB, INTEGER &INFO);

SUBROUTINE_F77 sgetrs3_(INTEGER &N, INTEGER &NRHS, REAL* A, INTEGER &LDA, 
		  INTEGER *IPIV, REAL *B, INTEGER &LDB, INTEGER &INFO);

void solve(int N, int NRHS, float *_A, float *_B)
{
   //CHARACTER TRANS("N",1);
   //INTEGER IPIV[N];
   INTEGER INFO;
   FMATRIX<float> A(_A, N, N);
   FMATRIX<float> B(_B, N, NRHS);
   
   //for (int i=0;i<N;i++)
   //   IPIV[i]=i+1;
   solve_(N, NRHS, A, B, INFO);
   //sgetrs_(TRANS, N, NRHS, A, N, IPIV, B, N, INFO);
   //sgetrs3_(N, NRHS, A, N, IPIV, B, N, INFO);
}

int main()
{
   float A[3][3] = {{1, 0, 0}, {0,1,0}, {0,0,1}};
   //float A[3][3] = {{1, 0.5, 0}, {0,1,0.5}, {0,0,1}};
   //float B[3][3] = {{1, 0, 0}, {0,1,0}, {0,0,1}};
   float B[2][3] = {{1, 2, 3}, {1.1, 2, 3.5}};
   //float A[10] = {1, 0, 0, 0,1,0, 0,0,1};
   //float B[10] = {1, 2, 3, 0,1,0, 0,0,1};

   solve (3, 2, &A[0][0], &B[0][0]);
   cerr << B[0][0] << " " << B[0][1] << " " << B[0][2] << " " << endl;
   cerr << B[1][0] << " " << B[1][1] << " " << B[1][2] << " " << endl;
   //solve (3, A, B);
   //cerr << B[0] << " " << B[1] << " " << B[2] << " " << endl;
}

