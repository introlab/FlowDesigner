// Copyright (C) 2000 Dominic Letourneau (dominic.letourneau@courrier.usherb.ca)

// FuzzySet.h: interface for the FuzzySet class.
//
//////////////////////////////////////////////////////////////////////
#ifndef _FUZZY_SET_H_
#define _FUZZY_SET_H_

#include <string>
#include <vector>
#include <map>
#include "FuzzyFunction.h"
#include "TrapezoidalFunction.h"
#include "TriangularFunction.h"
#include "BufferedNode.h"


class FuzzySet : public BufferedNode {

  friend class FuzzyModel;

public:
	
  //returns the index of the function in the set
  int find_function_by_index(const std::string &name);
    
  //adds a triangular function to the set
  void add_triangular_function (const std::string &name, float a, float b, float c);
  
  //adds a trapezoidal function to the set
  void add_trapezoidal_function (const std::string &name, float a, float b, float c, float d);
  
  //constructor with a name
  FuzzySet(const std::string &name);
  
  FuzzySet(std::string nodeName, ParameterSet params);
  
  FuzzySet();
  
  FuzzySet(std::istream &in);
  
  //destructor
  virtual ~FuzzySet();
  
  //evaluation of all the membership function at once
  Vector<float> & get_all_membership_evaluation(float x);
  
  //get the evaluation of the membership function of a given name
  float get_membership_evaluation(const std::string &name, float x);
  
  //accessor for the name
  const std::string& get_name(){return m_name;}
  
  //reset all functions inference stacks
  void reset() {
    for (int i = 0 ; i < m_functions.size(); i++) {
      object_cast<FuzzyFunction>(m_functions[i]).reset_inference_values();
    }
  }
  
  //retuns the internal vector of fuzzy functions
  Vector<ObjectRef> & get_member_functions() {return m_functions;}
  
  //returns the number of function in the set
  int get_function_count() {return m_functions.size();}
  
  //print the membership functions
  void print_functions(std::ostream &out);
  
  virtual void calculate(int output_id, int count, Buffer &out);
  
  virtual ObjectRef clone();
  
  virtual void printOn(std::ostream &out) const;
  
  virtual void readFrom(std::istream &in=std::cin);
  
 private:
  
  //returns the pointer of the function of a given name
  FuzzyFunction * find_function_by_name (const std::string &name);

  //the set name
  std::string m_name;
  
  //the vector of fuzzy functions
  Vector<ObjectRef> m_functions;
  
  //the evaluation vector (results of the evaluation of functions)
  Vector<float> m_evaluation;
  
  //the inputID
  int m_functionID;
  
  //the outputID
  int m_setID;
};

#endif
