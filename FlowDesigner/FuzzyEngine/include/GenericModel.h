// Copyright (C) 2000 Dominic Letourneau (doumdi@yahoo.com)

// GenericModel.h: interface for the GenericModel class.
//
//////////////////////////////////////////////////////////////////////

#ifndef _GENERIC_MODEL_H_
#define _GENERIC_MODEL_H_

#include "FuzzyModel.h"

class GenericModel : public FuzzyModel  {

public:

	//constructor
	GenericModel();

	GenericModel(const GenericModel& model);

	GenericModel(string nodeName, ParameterSet params);

	GenericModel(istream &in){readFrom(in);}

	//destructor
	virtual ~GenericModel();

	//fuzzy conjunction operator (min)
	virtual float conjunction(Vector<float> &c_values);

	//fuzzy disjunction operator (max)
	virtual float disjunction(Vector<float> &d_values);

	//Mamdani defuzzification 
	virtual Vector<float>& defuzzification();
	
	//cloning capability
	virtual ObjectRef clone();

	virtual void printOn(ostream &out=cout) const;
	
	virtual void readFrom(istream &in=cin);

private:

};
#endif 
