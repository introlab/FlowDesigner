// Copyright (C) 2008 Alberto Perotti
// Author: Alberto Perotti

#ifndef _CYCLICENCODER_H_
#define _CYCLICENCODER_H_

#include "BufferedNode.h"
#include "Buffer.h"
#include "Vector.h"
#include <stdlib.h>
#include <math.h>
#include "macros_math.h"
#include "GF2Polynomial.h"

using namespace std;

namespace FD {

class CyclicEncoder : public BufferedNode {
   
	int inputID;
	int outputID;

	int K;
	GF2Polynomial g;	// The generator polynomial.

	GF2Polynomial w, x;	// Information word, code word.

public:
	CyclicEncoder(string nodeName, ParameterSet params);
	virtual ~CyclicEncoder();

	void calculate(int output_id, int count, Buffer &out);
};

}	//namespace FD

#endif	// _CYCLICENCODER_H_
