// TrapezoidalFunction.cpp: implementation of the TrapezoidalFunction class.
//
//////////////////////////////////////////////////////////////////////

#include "TrapezoidalFunction.h"

//////////////////////////////////////////////////////////////////////
// Construction
//////////////////////////////////////////////////////////////////////

TrapezoidalFunction::TrapezoidalFunction(const string &name, float a, float b, float c, float d)
: FuzzyFunction(name),m_a(a),m_b(b),m_c(c),m_d(d) {

}
//////////////////////////////////////////////////////////////////////
// Destruction
//////////////////////////////////////////////////////////////////////

TrapezoidalFunction::~TrapezoidalFunction() {

}
//////////////////////////////////////////////////////////////////////
// Evaluation of the function
//////////////////////////////////////////////////////////////////////

float TrapezoidalFunction::evaluate(float x) {
	


	//standard condition
	if (m_a <= x && x < m_b) {
		return (x - m_a) / (m_b - m_a);
	}

	if (m_b <= x && x < m_c) {
		return 1.0;
	}
	
	if (m_c <= x && x < m_d)  {
		return (m_d - x) / (m_d - m_c);
	}


	//outside range
	return 0;
	
}

//////////////////////////////////////////////////////////////////////
// Evaluation of the Area
//////////////////////////////////////////////////////////////////////
float TrapezoidalFunction::get_area() {

	float scaling_factor;
	float area = 0;
	
	if (m_inference_values.size() != 1) {
		//standard scaling factor
		scaling_factor = 1;
	}
	else {
		//getting scaling_factor (membership) 
		scaling_factor = m_inference_values[0];
	}


	area = scaling_factor * ((m_d - m_a) + (m_d - m_b));

		
	return area;
}

//////////////////////////////////////////////////////////////////////
// Evaluation of the center of gravity
//////////////////////////////////////////////////////////////////////
float TrapezoidalFunction::get_center_of_gravity() {

	float cog = 0;

	float left_cog = (2*m_b + m_a) / 3;
	float left_area = (m_b - m_a);

	float middle_cog = (m_c + m_b) / 2;
	float middle_area = 2 * (m_c - m_b);

	float right_cog = (2*m_c + m_d) / 3;
	float right_area = (m_d - m_c);
	


	cog =  (left_cog * left_area + middle_cog * middle_area + right_cog * right_area);
	cog /= (left_area + middle_area + right_area);



	return cog;
}
