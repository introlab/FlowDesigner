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

	//destructor
	virtual ~GenericModel();

	//fuzzy conjunction operator (min)
	virtual float conjunction(vector<float> &c_values);

	//fuzzy disjunction operator (max)
	virtual float disjunction(vector<float> &d_values);

	//Mamdani defuzzification 
	virtual vector<pair<string,float> > & defuzzification();

private:

};

#endif 
