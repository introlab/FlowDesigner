// Copyright (C) 2000 Dominic Letourneau (doumdi@yahoo.com)

// TrapezoidalFunction.cc: implementation of the TrapezoidalFunction class.
//
//////////////////////////////////////////////////////////////////////

#include "TrapezoidalFunction.h"
#include "Vector.h"
#include "FuzzyOperators.h"

using namespace std;

namespace FD {


DECLARE_NODE(TrapezoidalFunction)
DECLARE_TYPE(TrapezoidalFunction)
/*Node
 *
 * @name TrapezoidalFunction
 * @category Fuzzy
 * @description A Fuzzy Function to be included in a FuzzySet
 *
 * @output_name FUNCTION
 * @output_description The FuzzyFunction object
 * @output_type Vector<ObjectRef>
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
 * @parameter_name D
 * @parameter_type float
 * @parameter_description D value
 *
 * @parameter_name NAME
 * @parameter_type string
 * @parameter_description The name of the function
 *
END*/

//////////////////////////////////////////////////////////////////////
// Construction
//////////////////////////////////////////////////////////////////////
TrapezoidalFunction::TrapezoidalFunction(const string &name, float a, 
					 float b, float c, float d)
: FuzzyFunction(name),m_a(a),m_b(b),m_c(c),m_d(d) {

}

ObjectRef TrapezoidalFunction::clone() {

  TrapezoidalFunction* my_clone = new TrapezoidalFunction(m_name,m_a,m_b,m_c,m_d);

  my_clone->get_inference_values() = get_inference_values();

  return ObjectRef(my_clone);
}


TrapezoidalFunction::TrapezoidalFunction(string nodeName, ParameterSet params)
   : FuzzyFunction(nodeName,params) {
   
  m_a  = dereference_cast<float>(parameters.get("A"));
  m_b  = dereference_cast<float>(parameters.get("B"));
  m_c  = dereference_cast<float>(parameters.get("C"));
  m_d  = dereference_cast<float>(parameters.get("D"));
  m_name = object_cast<String>(parameters.get("NAME"));
  

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
void TrapezoidalFunction::printOn(ostream &out) const {
  out << "<TrapezoidalFunction "<<endl;
  out << "<Name "<< m_name<<" >"<<endl;
  out << "<A "<<m_a<<" >"<<endl;
  out << "<B "<<m_b<<" >"<<endl;
  out << "<C "<<m_c<<" >"<<endl;
  out << "<D "<<m_d<<" >"<<endl;
  out<<" >\n";
}

void TrapezoidalFunction::readFrom(istream &in) {

   string tag;
   
   while (1)
   {
      char ch;
      in >> ch;
      if (ch == '>') break;

      else if (ch != '<') {
       throw new ParsingException ("TrapezoidalFunction::readFrom : Parse error: '<' expected");
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
      else if (tag == "D") {
	in >> m_d;
      }
      else {
	throw new ParsingException ("TrapezoidalFunction::readFrom : unknown argument: " + tag);
      }

      if (!in) throw new ParsingException ("TrapezoidalFunction::readFrom : Parse error trying to build " + tag);

      in >> tag;
      if (tag != ">") 
         throw new ParsingException ("TrapezoidalFunction::readFrom : Parse error: '>' expected ");
   }
}

Vector<float> TrapezoidalFunction::get_bounds() {

  Vector<float> bounds(4);

  bounds[0] = m_a;
  bounds[1] = m_b;
  bounds[2] = m_c;
  bounds[3] = m_d;
  
  return bounds;

}
}//namespace FD
