#include "fortran.h"
#include "lapackflow.h"

#include <iostream>



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
