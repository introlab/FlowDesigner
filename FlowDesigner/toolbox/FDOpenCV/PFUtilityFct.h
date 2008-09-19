#ifndef _PFUTILITYFCT_H_
#define _PFUTILITYFCT_H_

#include <stdlib.h>
#include <math.h>

namespace RobotFlow {
//
// Utility functions mainly for random numbers generation
//  Copyright (C) 2004 Jean-Marc Valin
//
#define PFUTIL_FLOGLOOKUP2SIZE 256
#define PFUTIL_FLOGLOOKUP2SHIFT 15

union PFUTIL_FloatManip {
	float f;
	unsigned int i;
};

void PFUTIL_build_flog_table();

float PFUTIL_flog(const float in);

float PFUTIL_randn();

float PFUTIL_ran();

int PFUTIL_find_range(float i_x, const float *i_cumul, int i_cumulSize);

}

#endif
