// TriangularFunction.h: interface for the TriangularFunction class.
//
//////////////////////////////////////////////////////////////////////
#ifndef _TRIANGULARFUNCTION_H_
#define _TRIANGULARFUNCTION_H_

#include "FuzzyFunction.h"
#include <string>

using namespace std;

class TriangularFunction : public FuzzyFunction  {

public:

	//constructor
	TriangularFunction(const string &name, float a, float b, float c);

	//destructor
	virtual ~TriangularFunction();

	//evaluation function
	virtual float evaluate(float index);

	//area evaluation
	virtual float get_area();

	//cog evaluation
	virtual float get_center_of_gravity();

	//lower limits of the function
	virtual float get_upper_bound() {return m_c;}

	//higher limit of the functions
	virtual float get_lower_bound() {return m_a;}


private:

	//function limits
	float m_a;
	float m_b;
	float m_c;

};

#endif 
