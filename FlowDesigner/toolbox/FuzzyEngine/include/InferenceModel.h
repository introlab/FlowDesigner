// Copyright (C) 2000 Dominic Letourneau (dominic.letourneau@courrier.usherb.ca)

// InferenceModel.h: interface for the InferenceModel class.
//
//////////////////////////////////////////////////////////////////////

#ifndef _INFERENCE_MODEL_H_
#define _INFERENCE_MODEL_H_

#include "FuzzyModel.h"

namespace FD {

class InferenceModel : public FuzzyModel  {

public:

	//constructor
	InferenceModel();

	InferenceModel(const InferenceModel& model);

	InferenceModel(std::string nodeName, ParameterSet params);

	InferenceModel(std::istream &in){readFrom(in);}

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

	virtual void printOn(std::ostream &out=std::cout) const;
	
	virtual void readFrom(std::istream &in=std::cin);

	virtual void calculate(int output_id, int count, Buffer &out);

private:

	int m_outputSetsID;

};

}//namespace FD

#endif 
