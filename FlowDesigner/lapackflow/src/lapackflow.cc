#include "fortran.h"
#include "lapackflow.h"

#include <iostream>


inline void solve(int N, int NRHS, float *_A, float *_B)
{
   INTEGER INFO;
   FMATRIX<float> A(_A, N, N);
   FMATRIX<float> B(_B, N, NRHS);   
   solve_(N, NRHS, A, B, INFO);
}

inline void eig(int N, float *_A, float *D, float *_V)
{
   INTEGER INFO;
   FMATRIX<float> A(_A, N, N);
   FMATRIX<float> V(_V, N, N);
   eig_(N, A, D, V);

   for (int i=0;i<3;i++)
   {
      for (int j=0;j<3;j++)
	 cout << A(i,j) << " ";
      cout << endl;
   }
   cout << endl;cout << endl;
   for (int i=0;i<3;i++)
   {
      for (int j=0;j<3;j++)
	 cout << V(i,j) << " ";
      cout << endl;
   }
}

/*
int main()
{
   float A[3][3] = {{1, .5, 0}, {0.5,1,0}, {0,0,1}};
   float B[2][3] = {{1, 2, 3}, {1.1, 2, 3.5}};
   float V[3][3] = {{1, 0.5, 0}, {0,1,0}, {0,0,1}};
   float D[3];

   //solve (3, 2, &A[0][0], &B[0][0]);
   //cerr << B[0][0] << " " << B[0][1] << " " << B[0][2] << " " << endl;
   //cerr << B[1][0] << " " << B[1][1] << " " << B[1][2] << " " << endl;

   eig(3,&A[0][0],D,&V[0][0]);
   cerr << D[0] << " " << D[1] << " " << D[2] << " " << endl;
}

*/
