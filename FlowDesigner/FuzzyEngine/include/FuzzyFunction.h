// Copyright (C) 2000 Dominic Letourneau (doumdi@yahoo.com)

// FuzzyFunction.h: interface for the FuzzyFunction class.
//
//////////////////////////////////////////////////////////////////////
#ifndef _FUZZY_FUNCTION_H_
#define _FUZZY_FUNCTION_H_

#include <string>
#include <vector>
#include "BufferedNode.h"
#include "Vector.h"

using namespace std;

class FuzzyFunction : public BufferedNode, public Object{

public:

  //friend istream& operator>> (istream &in, FuzzyFunction &function);

  //friend ostream& operator<< (ostream &out, FuzzyFunction &function);
  
  


  //accessor for the function name
  const string & get_name();
  
  virtual string get_type()=0;
  
  //virtual destructor
  virtual ~FuzzyFunction();
  
  //pure virtual function
  virtual float evaluate(float x) = 0;
  
  //the constructor with a function name
  FuzzyFunction(const string &name);
  
  FuzzyFunction(string nodeName, ParameterSet params);
  
  
  //reset the inference vector
  void reset_inference_values() {m_inference_values.resize(0);}
  
  //return the inference vector
  Vector<float> & get_inference_values() {return m_inference_values;}
  
  //push a value in the vector
  void push_inference_value(float value) {m_inference_values.push_back(value);}
  
  //computes the area (pure virtual function)
  virtual float get_area() = 0;
  
  //computes the center of gravity (pure virtual function)
  virtual float get_center_of_gravity() = 0;
  
  //lower limits of the function
  virtual float get_upper_bound() = 0;
  
  //higher limit of the functions
  virtual float get_lower_bound() = 0;
  
  //cloning capability
  virtual FuzzyFunction* clone() = 0;
  
  virtual void calculate(int output_id, int count, Buffer &out);
  
 protected:
  
  //the function name (linguistic term)
  string m_name;
  
  //the vector used for inference (temporary)
  Vector<float> m_inference_values;
  
  int m_functionID;
  
  //we shouldn't use this default constructor
  FuzzyFunction();
  
};



inline void Vector<FuzzyFunction*>::printOn(ostream &out) const {

  cerr<<"PrintOn called Vector<FuzzyFunction*>"<<endl;
  
  for (int i = 0; i < size(); i++) {
    cerr<<"calling printon"<<endl;
    operator[](i)->printOn(out);
  }
}

inline void Vector<FuzzyFunction*>::readFrom(istream &in) {

}


inline void Vector<FuzzyFunction*>::destroy() {

 for (Vector<FuzzyFunction*>::iterator iter = this->begin();
      iter != this->end(); iter++) {
   delete (*iter);
 }

 delete this;
}

#endif
