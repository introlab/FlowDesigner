// Copyright (C) 2008 Alberto Perotti
// Author: Alberto Perotti

#include "Object.h"
#include "ParameterSet.h"
#include "Vector.h"
#include <stdlib.h>
#include <math.h>
#include "macros_math.h"

using namespace std;

namespace FD {

class Constellation : public vector<complex<float> > {
   
	int nbits;
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
/*
	void printOn(ostream &out) const
	{
	   out << "<Constellation " << endl;
	   out << "<nbits " << nbits << ">" << endl;
	   out << "<nsignals " << nsignals << ">" << endl;
	   out << "<Es " << Es << ">" << endl;
	   out << "<modType " << type << ">" << endl;
	   out << ">\n";
	}
*/
};

}	//namespace FD
