#include <limits.h>
#include <gtest/gtest.h>
#include "Matrix.h"

TEST(MatrixTest,ALLOC_TEST)
{
	FD::Matrix<float> *myMatrix = new FD::Matrix<float>(10,10);
	EXPECT_EQ(myMatrix->ncols(),10);
	EXPECT_EQ(myMatrix->nrows(),10);
}
