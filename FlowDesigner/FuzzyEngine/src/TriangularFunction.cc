// TriangularFunction.cpp: implementation of the TriangularFunction class.
//
//////////////////////////////////////////////////////////////////////

#include "TriangularFunction.h"

//////////////////////////////////////////////////////////////////////
// Construction
//////////////////////////////////////////////////////////////////////

TriangularFunction::TriangularFunction(const string &name, float a, float b, float c)
: FuzzyFunction(name),m_a(a),m_b(b),m_c(c) {


}
//////////////////////////////////////////////////////////////////////
// Destruction
//////////////////////////////////////////////////////////////////////

TriangularFunction::~TriangularFunction()
{

}
//////////////////////////////////////////////////////////////////////
// Evaluation
//////////////////////////////////////////////////////////////////////

float TriangularFunction::evaluate(float x) {
	
	if (m_a <= x && x <= m_b) {
		return (x - m_a) / (m_b - m_a);
	}
	
	if (m_b <= x && x <= m_c) {
		return (m_c - x) / (m_c - m_b);
	}

	return 0;
}

//////////////////////////////////////////////////////////////////////
// Evaluation of the Area
//////////////////////////////////////////////////////////////////////
float TriangularFunction::get_area() {

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


	area = scaling_factor * ((m_c - m_a) / 2.0);

		
	return area;
}

//////////////////////////////////////////////////////////////////////
// Evaluation of the center of gravity
//////////////////////////////////////////////////////////////////////
float TriangularFunction::get_center_of_gravity() {

	float cog = 0;

	//this should be optimized
	float left_area = (m_b - m_a);
	float right_area = (m_c - m_b);

	float center_left = (2 * m_b + m_a) / 3;
	float center_right = (2 * m_b + m_c) / 3;
	
	cog = (left_area * center_left + right_area * center_right) / (left_area + right_area);

	return cog;
}
