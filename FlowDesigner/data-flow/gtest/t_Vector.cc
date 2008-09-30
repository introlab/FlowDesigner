#include <limits.h>
#include <gtest/gtest.h>
#include "Vector.h"

TEST(VectorTest,ALLOC_TEST)
{
	FD::Vector<float> *myVect = FD::Vector<float>::alloc(10);
	EXPECT_EQ(myVect->size(),10);
	
}