// Copyright (C) 2008 Alberto Perotti
// Author: Alberto Perotti

#ifndef _CONSTELLATIONMAPPER_H_
#define _CONSTELLATIONMAPPER_H_

#include "BufferedNode.h"
#include "Buffer.h"
#include "Vector.h"
#include <stdlib.h>
#include <math.h>
#include "macros_math.h"
#include "Constellation.h"

using namespace std;

namespace FD {

class ConstellationMapper : public BufferedNode {
   
	int inputID;
	int outputID;
	int nbits;
	// int nsignals;
	// float Es;
	// enum modType {PAM, PSK, QAM, FILE_T};
	// modType type;
	// RCPtr<Vector<complex<float> > > Xiptr;
	Constellation Xi;
	// string filename;

public:
	ConstellationMapper(string nodeName, ParameterSet params);
	virtual ~ConstellationMapper();

	void calculate(int output_id, int count, Buffer &out);
};

}//namespace FD

#endif	// _CONSTELLATIONMAPPER_H_
