// Copyright (C) 2008 Alberto Perotti
// Author: Alberto Perotti

#ifndef _LLRCOMPUTER_H_
#define _LLRCOMPUTER_H_

#include "BufferedNode.h"
#include "Buffer.h"
#include "Vector.h"
#include <stdlib.h>
#include <math.h>
#include "macros_math.h"
#include "Constellation.h"

using namespace std;

namespace FD {

class LLRComputer : public BufferedNode {
   
	int inputID;
	int outputID;
	int d_nbits;
	int nsignals;
	float Es;
	enum modType {PAM, PSK, QAM, FILE_T};
	modType type;
	// RCPtr<Vector<complex<float> > > Xiptr;
	Constellation Xi;
	string filename;
	float sigma;
	bool bit_llrs;
	vector<float> tllr;

public:
	LLRComputer(){ ParameterSet ps;};
	LLRComputer(string nodeName, ParameterSet params);

	void calculate(int output_id, int count, Buffer &out);
	float calculate(complex<float> s, int bit);	// TODO: should return a vector<float>

	int nbits(){ return d_nbits;};
};	// class LLRComputer

}	// namespace FD

#endif	// _LLRCOMPUTER_H_
