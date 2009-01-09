// Copyright (C) 2008 Alberto Perotti
// Author: Alberto Perotti

#ifndef _CONSTELLATION_H_
#define _CONSTELLATION_H_

#include "Object.h"
#include "ParameterSet.h"
#include "Vector.h"
#include <stdlib.h>
#include <math.h>
#include "macros_math.h"

using namespace std;

namespace FD {

class Constellation : public vector<complex<float> > {
   
	int d_nbits;
	int nsignals;
	float Es;
	enum modType {PAM, PSK, QAM, FILE_T};
	modType type;
	string filename;

	void c_initialize(ParameterSet &params);

public:
	Constellation();
	Constellation(ParameterSet params);
	virtual ~Constellation(){};

	int nbits() const {return d_nbits;}
};

}	//namespace FD

#endif	// _CONSTELLATION_H_
