// Copyright (C) 2000 Dominic Letourneau (doumdi@yahoo.com)

// FuzzySet.cc: implementation of the FuzzySet class.
//
//////////////////////////////////////////////////////////////////////

#include "FuzzySet.h"
#include "Exception.h"
#include <stdlib.h>
#include <stdio.h>
#include <string>
#include <iostream>
#include "Vector.h"
#include "FuzzyOperators.h"

DECLARE_NODE(FuzzySet)
DECLARE_TYPE(FuzzySet)
/*Node
 *
 * @name FuzzySet
 * @category Fuzzy
 * @description A FuzzySet containing functions associated with names
 *
 * @input_name FUNCTIONS
 * @input_description The Fuzzy Functions
 * @input_type Vector<ObjectRef>
 *
 * @output_name SET
 * @output_description The FuzzySet with multiple Fuzzy Functions
 * @output_type Vector<ObjectRef>
 *
 * @parameter_name NAME
 * @parameter_description The name of the set
 * @parameter_type string
 *
END*/


//////////////////////////////////////////////////////////////////////
// Construction
//////////////////////////////////////////////////////////////////////

FuzzySet::FuzzySet(const string &name)
  : BufferedNode("INVALID", ParameterSet()), m_name(name) {

}

FuzzySet::FuzzySet(istream &in) 
  : BufferedNode("INVALID", ParameterSet()), m_name("INVALID") {
  readFrom(in);
}


FuzzySet::FuzzySet() 
  : BufferedNode("INVALID", ParameterSet()), m_name("INVALID") {
}

FuzzySet::FuzzySet(string nodeName, ParameterSet params) 
  : BufferedNode(nodeName,params) {
  
  
  m_name = object_cast<String>(parameters.get("NAME"));
  
  //the inputID
  m_functionID = addInput("FUNCTIONS");

  //the outputID
  m_setID = addOutput("SET");
}

//////////////////////////////////////////////////////////////////////
// Destruction
//////////////////////////////////////////////////////////////////////

FuzzySet::~FuzzySet() {
  
  m_functions.resize(0);
}
//////////////////////////////////////////////////////////////////////
// add a trapezoid function
//////////////////////////////////////////////////////////////////////

void FuzzySet::add_trapezoidal_function(const string &name, float a, 
					float b, float c, float d) {

  m_functions.push_back(ObjectRef(new TrapezoidalFunction(name,a,b,c,d)));	
  m_evaluation.resize(m_functions.size());
}

//////////////////////////////////////////////////////////////////////
// add a triangulare function
//////////////////////////////////////////////////////////////////////

void FuzzySet::add_triangular_function(const string &name, float a, float b, float c) {

  m_functions.push_back(ObjectRef(new TriangularFunction(name,a,b,c)));

  m_evaluation.resize(m_functions.size());

}

//////////////////////////////////////////////////////////////////////
// evaluate all membership function
//////////////////////////////////////////////////////////////////////

Vector<float> & FuzzySet::get_all_membership_evaluation(float x) {
  
  //updating evaluation vector
  m_evaluation.resize(m_functions.size());
  
  
  for (int i = 0; i < m_functions.size(); i++) {
    m_evaluation[i] = object_cast<FuzzyFunction>(m_functions[i]).evaluate(x);

  }
  
  return m_evaluation;
  
}

//////////////////////////////////////////////////////////////////////
// Returns the evaluation of the given named function
//////////////////////////////////////////////////////////////////////

float FuzzySet::get_membership_evaluation(const string &name, float x) {
  
  
  int index = find_function_by_index(name);
  
  m_evaluation[index] = object_cast<FuzzyFunction>(m_functions[index]).evaluate(x);
  
  return m_evaluation[index];
}


//////////////////////////////////////////////////////////////////////
// Returns the index of a given function name
//////////////////////////////////////////////////////////////////////

int FuzzySet::find_function_by_index(const string &name) {
  
  for (int i = 0; i < m_functions.size(); i++) {
    
    if (object_cast<FuzzyFunction>(m_functions[i]).get_name() == name) return i;
    
  }
  
  char message[256];
  
  sprintf(message,"CANNOT GET MEMBERSHIP FUNCTION CALLED : %s",name.c_str());
  
  throw new GeneralException(message,__FILE__,__LINE__);
  
  return -1;
}

//////////////////////////////////////////////////////////////////////
// Returns the pointer of a given function name
//////////////////////////////////////////////////////////////////////

FuzzyFunction* FuzzySet::find_function_by_name (const string &name) {
  
  for (int i = 0; i < m_functions.size(); i++) {
    
    if (object_cast<FuzzyFunction>(m_functions[i]).get_name() == name) 
      return dynamic_cast<FuzzyFunction*>(m_functions[i].get());
    
  }
  
  char message[256];
  
  sprintf(message,"CANNOT GET MEMBERSHIP FUNCTION CALLED : %s",name.c_str());
  
  throw new GeneralException(message,__FILE__,__LINE__);
  
  return NULL;
  
}
//////////////////////////////////////////////////////////////////////
// Printing the functions (graphics)
//////////////////////////////////////////////////////////////////////

void FuzzySet::print_functions(ostream &out) {
  
  if (m_functions.size() > 0) {
    float min = object_cast<FuzzyFunction>(m_functions[0]).get_lower_bound(); 
    float max = object_cast<FuzzyFunction>(m_functions[0]).get_upper_bound();
    int i;
    
    //finding limits
    for (i = 0; i < m_functions.size(); i++) {
      
      if (min > object_cast<FuzzyFunction>(m_functions[i]).get_lower_bound()) {
	min = object_cast<FuzzyFunction>(m_functions[i]).get_lower_bound();
      }
      
      if (max < object_cast<FuzzyFunction>(m_functions[i]).get_upper_bound()) {
	max = object_cast<FuzzyFunction>(m_functions[i]).get_upper_bound();
      }
      
      
    }
    
    //printing membership values
    for (float index = min; index <= max; index++) {
      
      //printing membership function names
      for (i = 0; i < m_functions.size(); i++) {
	out<<object_cast<FuzzyFunction>(m_functions[i]).get_name()<<"\t";	
      }
      
      out<<index<<"\t";
      
      
      //printing membership function values
      for (i = 0; i < m_functions.size(); i++) {
	out<<object_cast<FuzzyFunction>(m_functions[i]).evaluate(index)<<"\t";
      }
      
      
      out<<endl;
    }
    
  }
}

//////////////////////////////////////////////////////////////////////
// calculate
//////////////////////////////////////////////////////////////////////

void FuzzySet::calculate(int output_id, int count, Buffer &out) {


  //cerr<<"FuzzySet Calculate"<<endl;
  m_functions.resize(0);


  //getting functions 
  ObjectRef Functions = getInput(m_functionID, count);
  Vector<ObjectRef> &funct_vect = object_cast<Vector<ObjectRef> >(Functions);

  
  for (int i = 0 ; i < funct_vect.size(); i++) {
    m_functions.push_back(funct_vect[i]->clone());
  }

  out[count] = ObjectRef(new Vector<ObjectRef>(1,clone()));

}

ObjectRef FuzzySet::clone() {

  FuzzySet* my_set = new FuzzySet(m_name);

  for (int i = 0; i < m_functions.size(); i++) {
    my_set->m_functions.push_back(m_functions[i]->clone());
  }

  return ObjectRef(my_set);
}

void FuzzySet::printOn(ostream &out) const {

  out << "<FuzzySet "<<endl; 
  out << "<Name "<<m_name<<" >"<<endl;
  out << "<Size "<<m_functions.size()<<" >"<<endl;

  for (int i = 0; i < m_functions.size(); i++) {
    out<<"<Function "<<m_functions[i]<<" >"<<endl;
  }
  out <<" >\n";
}

void FuzzySet::readFrom(istream &in) {

   string tag;
   int size;

   while (1)
   {
      char ch;
      in >> ch;
      if (ch == '>') break;

      else if (ch != '<') {
       throw new ParsingException ("Parse error: '<' expected");
      }
      in >> tag;

      if (tag == "Name") {
         in >> m_name;
      }
      else if (tag == "Size") {
         in >> size;
	 m_evaluation.resize(size);
      }
      else if (tag == "Function") {
	//reading function
	ObjectRef value;
	in>>value;
	m_functions.push_back(value);
      }
      else {
	throw new ParsingException ("unknown argument: " + tag);
      }

      if (!in) throw new ParsingException ("Parse error trying to build " + tag);

      in >> tag;
      if (tag != ">") 
         throw new ParsingException ("Parse error: '>' expected ");
   }
}
