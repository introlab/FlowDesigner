// Copyright (C) 2000 Dominic Letourneau (dominic.letourneau@courrier.usherb.ca)

// TrapezoidalFunction.h: interface for the TrapezoidalFunction class.
//
//////////////////////////////////////////////////////////////////////
#ifndef _TRAPEZOIDALFUNCTION_H_
#define _TRAPEZOIDALFUNCTION_H_


#include <string>
#include "FuzzyFunction.h"


namespace FD {

class TrapezoidalFunction : public FuzzyFunction  {

public:

  virtual std::string get_type() {return std::string("Trapezoidal");}
  
  //evaluation of the function 
  virtual float evaluate(float x);
  
  //constructor of the function
  TrapezoidalFunction(const std::string &name, float a, float b, float c, float d);
  
  TrapezoidalFunction(std::string nodeName, ParameterSet params);

  TrapezoidalFunction(std::istream &in) {readFrom(in);}

  TrapezoidalFunction() {}
  
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
  float m_d;
  
};

}//namespace FD

#endif 
