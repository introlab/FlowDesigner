// Copyright (C) 2000 Dominic Letourneau (doumdi@yahoo.com)

// InferenceModel.h: interface for the InferenceModel class.
//
//////////////////////////////////////////////////////////////////////

#ifndef _INFERENCE_MODEL_H_
#define _INFERENCE_MODEL_H_

#include "FuzzyModel.h"

class InferenceModel : public FuzzyModel  {

public:

  

	//constructor
	InferenceModel();

	InferenceModel(const InferenceModel& model);

	InferenceModel(string nodeName, ParameterSet params);

	InferenceModel(istream &in){readFrom(in);}

	//destructor
	virtual ~InferenceModel();

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

	virtual void calculate(int output_id, int count, Buffer &out);

private:

	int m_outputSetsID;

};
#endif 
