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

// FuzzySet.cc: implementation of the FuzzySet class.
//
//////////////////////////////////////////////////////////////////////

#include "FuzzySet.h"
#include "Exception.h"
#include <stdlib.h>
#include <stdio.h>
#include <string>
#include <iostream>


DECLARE_NODE(FuzzySet)
/*Node
 *
 * @name FuzzySet
 * @category Fuzzy
 * @description No description available
 *
 * @input_name TRAIN_IN
 * @input_description No description available
 *
 * @output_name OUTPUT
 * @output_description No description available
 *
 * @parameter_name BATCH_SETS
 * @parameter_description No description available
 *
END*/


//////////////////////////////////////////////////////////////////////
// Construction
//////////////////////////////////////////////////////////////////////

FuzzySet::FuzzySet(const string &name)
:m_name(name) {

}
FuzzySet::FuzzySet(string nodeName, ParameterSet params) 
: BufferedNode(nodeName,params),m_name(nodeName) {
  

}

//////////////////////////////////////////////////////////////////////
// Destruction
//////////////////////////////////////////////////////////////////////

FuzzySet::~FuzzySet() {
  
  for (int i = 0; i < m_functions.size(); i++) {
    delete m_functions[i];
  }
  
  m_functions.resize(0);
}
//////////////////////////////////////////////////////////////////////
// add a trapezoid function
//////////////////////////////////////////////////////////////////////

void FuzzySet::add_trapezoidal_function(const string &name, float a, 
					float b, float c, float d) {

  m_functions.push_back(new TrapezoidalFunction(name,a,b,c,d));	
}

//////////////////////////////////////////////////////////////////////
// add a triangulare function
//////////////////////////////////////////////////////////////////////

void FuzzySet::add_triangular_function(const string &name, float a, float b, float c) {
  m_functions.push_back(new TriangularFunction(name,a,b,c));
}

//////////////////////////////////////////////////////////////////////
// evaluate all membership function
//////////////////////////////////////////////////////////////////////

vector<float> & FuzzySet::get_all_membership_evaluation(float x) {
  
  //updating evaluation vector
  m_evaluation.resize(m_functions.size());
  
  //updating maps
  m_string_value_map.clear();
  
  
  
  for (int i = 0; i < m_functions.size(); i++) {
    m_evaluation[i] = m_functions[i]->evaluate(x);
    
    m_string_value_map.insert(
			      pair<string,float>(m_functions[i]->get_name(),m_evaluation[i]));
    
  }
  
  return m_evaluation;
  
}

//////////////////////////////////////////////////////////////////////
// Returns the evaluation of the given named function
//////////////////////////////////////////////////////////////////////

float FuzzySet::get_membership_evaluation(const string &name, float x) {
  
  
  int index = find_function_by_index(name);
  
  m_evaluation[index] = m_functions[index]->evaluate(x);
  
  m_string_value_map.insert(
			    pair<string,float>(name,m_evaluation[index]));
  
  return m_evaluation[index];
}


//////////////////////////////////////////////////////////////////////
// Returns the index of a given function name
//////////////////////////////////////////////////////////////////////

int FuzzySet::find_function_by_index(const string &name) {
  
  for (int i = 0; i < m_functions.size(); i++) {
    
    if (m_functions[i]->get_name() == name) return i;
    
  }
  
  char message[256];
  
  sprintf(message,"CANNOT GET MEMBERSHIP FUNCTION CALLED : %s",name.c_str());
  
  throw GeneralException(message,__FILE__,__LINE__);
  
  return -1;
}

//////////////////////////////////////////////////////////////////////
// Returns the pointer of a given function name
//////////////////////////////////////////////////////////////////////

FuzzyFunction * FuzzySet::find_function_by_name (const string &name) {
  
  for (int i = 0; i < m_functions.size(); i++) {
    
    if (m_functions[i]->get_name() == name) return m_functions[i];
    
  }
  
  char message[256];
  
  sprintf(message,"CANNOT GET MEMBERSHIP FUNCTION CALLED : %s",name.c_str());
  
  throw GeneralException(message,__FILE__,__LINE__);
  
  return NULL;
  
}
//////////////////////////////////////////////////////////////////////
// Printing the functions (graphics)
//////////////////////////////////////////////////////////////////////

void FuzzySet::print_functions(ostream &out) {
  
  if (!m_functions.empty()) {
    float min = m_functions[0]->get_lower_bound(); 
    float max = m_functions[0]->get_upper_bound();
    int i;
    
    //finding limits
    for (i = 0; i < m_functions.size(); i++) {
      
      if (min > m_functions[i]->get_lower_bound()) {
	min = m_functions[i]->get_lower_bound();
      }
      
      if (max < m_functions[i]->get_upper_bound()) {
	max = m_functions[i]->get_upper_bound();
      }
      
      
    }
    
    //printing membership values
    for (float index = min; index <= max; index++) {
      
      //printing membership function names
      for (i = 0; i < m_functions.size(); i++) {
	out<<m_functions[i]->get_name()<<"\t";	
      }
      
      out<<index<<"\t";
      
      
      //printing membership function values
      for (i = 0; i < m_functions.size(); i++) {
	out<<m_functions[i]->evaluate(index)<<"\t";
      }
      
      
      out<<endl;
    }
    
  }
}

//////////////////////////////////////////////////////////////////////
// calculate
//////////////////////////////////////////////////////////////////////

void FuzzySet::calculate(int output_id, int count, Buffer &out) {



}
