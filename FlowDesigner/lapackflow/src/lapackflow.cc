#include "fortran.h"
#include "lapackflow.h"

#include <iostream>




int main()
{
   float A[3][3] = {{1, 0.5, 0}, {0,1,0}, {0,0,1}};
   float B[2][3] = {{1, 2, 3}, {1.1, 2, 3.5}};

   solve (3, 2, &A[0][0], &B[0][0]);
   cerr << B[0][0] << " " << B[0][1] << " " << B[0][2] << " " << endl;
   cerr << B[1][0] << " " << B[1][1] << " " << B[1][2] << " " << endl;
}

