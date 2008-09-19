//
// Utility functions mainly for random numbers generation
//  Copyright (C) 2004 Jean-Marc Valin
//

#include "PFUtilityFct.h"

using namespace std;

namespace RobotFlow {

static float PFUTIL_logtable2[PFUTIL_FLOGLOOKUP2SIZE];

void PFUTIL_build_flog_table()
{
	static bool init=false;
	if (!init) {
		PFUTIL_FloatManip m;
		
		for (int i=0;i<PFUTIL_FLOGLOOKUP2SIZE;i++) {
			m.i = (i<<PFUTIL_FLOGLOOKUP2SHIFT) | 0x3f800000;
			PFUTIL_logtable2[i]=log(m.f);
		}
		
		init=true;
	}
}

float PFUTIL_flog(const float in)
{
	PFUTIL_build_flog_table();
	PFUTIL_FloatManip m;
	m.f = in;
	
	//Extract the mantissa and perform lookup for log(mantissa)
	float tb = PFUTIL_logtable2[(m.i & 0x007fffff)>>PFUTIL_FLOGLOOKUP2SHIFT];
	//Extract the exponent
	int id = (m.i>>23)-127;
	
	return id*M_LN2 + tb;
}

float PFUTIL_randn()
{
	static bool avail = false;
	static float cached=0.f;
	
	if (avail) {
		avail = false;
		return cached;
	} 
	else {
		float U1, U2, S, x;
		do {
			U1 = float(rand())/float(RAND_MAX);
			U2 = float(rand())/float(RAND_MAX);
			U1 = 2*U1-1;
			U2 = 2*U2-1;
			S = U1*U1 + U2*U2;
		} while (S >= 1 || S == 0.0f);
		
		float tmp = sqrt(-2 * PFUTIL_flog(S) / S);
		cached = tmp * U1;
		avail = true;
		
		return tmp * U2;
	}
}

float PFUTIL_ran()
{
	const unsigned int jflone = 0x3f800000;
	const unsigned int jflmsk = 0x007fffff;
	static int idum = 10000;
	union {int i; float f;} ran;
	
	idum = 1664525*idum + 1013904223;
	ran.i = jflone | (jflmsk & idum);
	ran.f -= 1;
	
	return ran.f;
}

int PFUTIL_find_range(float i_x, const float *i_cumul, int i_cumulSize) 
{
	int low=0;
	int high=i_cumulSize-1;
	int cur = high>>1;
	
	while (1) {
		if (i_cumul[cur] > i_x) {
			if (cur == 0 || i_cumul[cur-1] <= i_x) {
				break;
			} 
			else {
				high = cur-1;
				cur = (low+cur) >> 1;
			}
		} 
		else {
			low = cur;
			cur = (high+cur+1) >> 1;
			
			if (cur==high) {
				break;
			}
		}
	}

	return cur;
}

}
