// Copyright (C) 2000 Dominic Letourneau (doumdi@yahoo.com)
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2, or (at your option)
// any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this file.  If not, write to the Free Software Foundation,
// 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
// TriangularFunction.cc: implementation of the TriangularFunction class.
//
//////////////////////////////////////////////////////////////////////

#include "TriangularFunction.h"
#include "Vector.h"
#include "FuzzyOperators.h"

DECLARE_NODE(TriangularFunction)
/*Node
 *
 * @name TriangularFunction
 * @category Fuzzy
 * @description No description available
 *
 * @parameter_name A
 * @parameter_type float
 * @parameter_description A value
 *
 * @parameter_name B
 * @parameter_type float
 * @parameter_description B value
 *
 * @parameter_name C
 * @parameter_type float
 * @parameter_description C value
 *
 * @output_name FUNCTION
 * @output_description The FuzzyFunction object
 * @output_type FuzzyFunction
 *
 * @parameter_name NAME
 * @parameter_type string
 * @parameter_description The name of the function
 *
END*/


//////////////////////////////////////////////////////////////////////
// Construction
//////////////////////////////////////////////////////////////////////

TriangularFunction::TriangularFunction(const string &name, float a, float b, float c)
: FuzzyFunction(name),m_a(a),m_b(b),m_c(c) {


}

FuzzyFunction* TriangularFunction::clone() {
  return new TriangularFunction(m_name,m_a,m_b,m_c);
}


TriangularFunction::TriangularFunction(string nodeName, ParameterSet params)
 : FuzzyFunction(nodeName,params) {

  m_a  = dereference_cast<float>(parameters.get("A"));
  m_b  = dereference_cast<float>(parameters.get("B"));
  m_c  = dereference_cast<float>(parameters.get("C"));
  m_name = dereference_cast<string>(parameters.get("NAME"));
  
}

//////////////////////////////////////////////////////////////////////
// Destruction
//////////////////////////////////////////////////////////////////////

TriangularFunction::~TriangularFunction() {

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
//////////////////////////////////////////////////////////////////////
// calculate
//////////////////////////////////////////////////////////////////////
void TriangularFunction::calculate(int output_id, int count, Buffer &out) {

  out[count] = ObjectRef(new Vector<FuzzyFunction*>(1,clone()));

}
