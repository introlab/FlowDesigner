// Copyright (C) 2000 Dominic Letourneau (doumdi@yahoo.com)

#include "TriangularFunction.h"
#include "Vector.h"
#include "FuzzyOperators.h"

using namespace std;
using namespace FD;

DECLARE_NODE(TriangularFunction)
DECLARE_TYPE(TriangularFunction)
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
 * @output_type Vector<ObjectRef>
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

ObjectRef TriangularFunction::clone() {

  TriangularFunction* my_clone = new TriangularFunction(m_name,m_a,m_b,m_c);

  my_clone->get_inference_values() = get_inference_values();

  return ObjectRef(my_clone);
}


TriangularFunction::TriangularFunction(string nodeName, ParameterSet params)
 : FuzzyFunction(nodeName,params) {

  m_a  = dereference_cast<float>(parameters.get("A"));
  m_b  = dereference_cast<float>(parameters.get("B"));
  m_c  = dereference_cast<float>(parameters.get("C"));
  m_name = object_cast<String>(parameters.get("NAME"));
  
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

void TriangularFunction::printOn(ostream &out) const {
  out << "<TriangularFunction "<<endl;
  out << "<Name "<< m_name<<" >"<<endl;
  out << "<A "<<m_a<<" >"<<endl;
  out << "<B "<<m_b<<" >"<<endl;
  out << "<C "<<m_c<<" >"<<endl;
  out<<" >\n";
}


void TriangularFunction::readFrom(istream &in) {
  
  string tag;
  
  while (1) {
    char ch;
    in >> ch;
    if (ch == '>') break;
    
    else if (ch != '<') {
      throw new ParsingException ("TriangularFunction::readFrom : Parse error: '<' expected");
    }
    in >> tag;
    
    if (tag == "Name") {
      in >> m_name;
    }
    else if (tag == "A") {
      in >> m_a;
    }
    else if (tag == "B") {
      in >> m_b;
    }
    else if (tag == "C") {
      in >> m_c;
    }
    else {
      throw new ParsingException ("TriangularFunction::readFrom : unknown argument: " + tag);
    }
    
    if (!in) throw new ParsingException ("TriangularFunction::readFrom : Parse error trying to build " + tag);
    
    in >> tag;
    if (tag != ">") 
      throw new ParsingException ("TriangularFunction::readFrom : Parse error: '>' expected ");
  }
  
}

Vector<float> TriangularFunction::get_bounds() {

  Vector<float> bounds(3);

  bounds[0] = m_a;
  bounds[1] = m_b;
  bounds[2] = m_c;
  
  return bounds;

}
