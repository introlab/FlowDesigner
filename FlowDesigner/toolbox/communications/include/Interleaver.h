// Copyright (C) 2008 Alberto Perotti
// Author: Alberto Perotti

#ifndef _INTERLEAVER_H_
#define _INTERLEAVER_H_

#include "BufferedNode.h"
#include "Buffer.h"
#include "Vector.h"

using namespace std;

namespace FD {

class Interleaver : public BufferedNode {
protected:   
	int inputID;
	int outputID;
	int length;
	Vector<int> perm;
	bool inverse;

public:
	Interleaver(string nodeName, ParameterSet params);
	void calculate(int output_id, int count, Buffer &out);
};

}//namespace FD
#endif	// _INTERLEAVER_H_
