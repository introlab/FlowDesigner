// Copyright (C) 2000 Dominic Letourneau (doumdi@yahoo.com)

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

  virtual string get_type() {return string("Trapezoidal");}
  
  //evaluation of the function 
  virtual float evaluate(float x);
  
  //constructor of the function
  TrapezoidalFunction(const string &name, float a, float b, float c, float d);
  
  TrapezoidalFunction(string nodeName, ParameterSet params);

  TrapezoidalFunction(istream &in) {readFrom(in);}

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
  
  //cloning capability
  virtual FuzzyFunction* clone();
  
  virtual void printOn(ostream &out=cout) const;
  
  virtual void readFrom(istream &in=cin);
  
 private:
  
  //function limits
  float m_a;
  float m_b;
  float m_c;
  float m_d;
  
};

#endif 
