// Copyright (C) 2000 Dominic Letourneau (dominic.letourneau@courrier.usherb.ca)

// TriangularFunction.h: interface for the TriangularFunction class.
//
//////////////////////////////////////////////////////////////////////
#ifndef _TRIANGULARFUNCTION_H_
#define _TRIANGULARFUNCTION_H_

#include "FuzzyFunction.h"
#include <string>


class TriangularFunction : public FuzzyFunction  {

public:

  virtual std::string get_type() {return std::string("Triangular");}
  
  //constructor
  TriangularFunction(const std::string &name, float a, float b, float c);

  TriangularFunction(std::string nodeName, ParameterSet params);

  TriangularFunction(std::istream &in) {readFrom(in);}

  TriangularFunction() {}
  
  //destructor
  virtual ~TriangularFunction();
  
  //evaluation function
  virtual float evaluate(float index);
  
  //area evaluation
  virtual float get_area();
  
  //cog evaluation
  virtual float get_center_of_gravity();
  
  //higher limits of the function
  virtual float get_upper_bound() {return m_c;}
  
  //lower limit of the functions
  virtual float get_lower_bound() {return m_a;}

  //get all bounds of the function
  virtual Vector<float> get_bounds();
 
  //cloning capability
  virtual ObjectRef clone();
  
  virtual void printOn(std::ostream &out=std::cout) const;
  
  virtual void readFrom(std::istream &in=std::cin);
  
 private:
  
  //function limits
  float m_a;
  float m_b;
  float m_c;
  
};

#endif 
