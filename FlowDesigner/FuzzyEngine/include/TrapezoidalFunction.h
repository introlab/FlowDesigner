// TrapezoidalFunction.h: interface for the TrapezoidalFunction class.
//
//////////////////////////////////////////////////////////////////////
#ifndef _TRAPEZOIDALFUNCTION_H_
#define _TRAPEZOIDALFUNCTION_H_


#include <string>
#include "FuzzyFunction.h"

using namespace std;

class TrapezoidalFunction : public FuzzyFunction  {

public:

	//evaluation of the function 
	virtual float evaluate(float x);

	//constructor of the function
	TrapezoidalFunction(const string &name, float a, float b, float c, float d);

	//destructor
	virtual ~TrapezoidalFunction();

	//area evaluation
	virtual float get_area();

	//cog evaluation
	virtual float get_center_of_gravity();

	//lower limits of the function
	virtual float get_upper_bound() {return m_d;}

	//higher limit of the functions
	virtual float get_lower_bound() {return m_a;}

private:

	//function limits
	float m_a;
	float m_b;
	float m_c;
	float m_d;

};

#endif 
