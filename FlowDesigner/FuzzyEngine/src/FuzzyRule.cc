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

// FuzzyRule.cc: implementation of the FuzzyRule class.
//
//////////////////////////////////////////////////////////////////////

#include "FuzzyRule.h"
#include <string>


DECLARE_NODE(FuzzyRule)
/*Node
 *
 * @name FuzzyRule
 * @category Fuzzy
 * @description A Rule containing ANTECEDANTS (IF) and CONSEQUENTS(THEN)
 *
 * @parameter_name IF
 * @parameter_description Antecedant of the rule seperated by spaces
 * @parameter_type string
 *
 * @parameter_name THEN
 * @parameter_description Consequent of the rule seperated by spaces
 * @parameter_type string
 * 
 * @output_name RULE
 * @output_description The FuzzyRule Object
 * @output_type FuzzyRule
 *
END*/

//////////////////////////////////////////////////////////////////////
// Construction
//////////////////////////////////////////////////////////////////////
  
FuzzyRule::FuzzyRule() 
  :BufferedNode("INVALID",ParameterSet()), m_rule_number(-1) {
  
}

FuzzyRule::FuzzyRule(int rule_number)
  : BufferedNode("INVALID",ParameterSet()), m_rule_number(rule_number) {

}

FuzzyRule::FuzzyRule(const FuzzyRule& cpy) 
  :BufferedNode(cpy.name, ParameterSet()), m_rule_number(cpy.m_rule_number) {

  for (int i = 0; i < cpy.m_antecedant.size(); i++) {
    m_antecedant.push_back(cpy.m_antecedant[i]);
  }

  for (int i = 0; i < cpy.m_consequent.size(); i++) {
    m_consequent.push_back(cpy.m_consequent[i]);
  }

}

FuzzyRule::FuzzyRule(string nodeName, ParameterSet params) 
: BufferedNode(nodeName,params) {
  
  
  String antecedant  = object_cast<String>(parameters.get("IF"));
  String consequent  = object_cast<String>(parameters.get("THEN"));


  vector<string> tmp_vector;

  char *tmp_string = strtok(const_cast<char*>(antecedant.c_str()),": \n");
  
  do {
    tmp_vector.push_back(string(tmp_string));
    tmp_string = strtok(NULL,": \n");
  }
  while (tmp_string != NULL);

  if (tmp_vector.size() %2 == 0) {
    for (int i = 0; i < tmp_vector.size(); i+=2) {
      m_antecedant.push_back(make_pair(tmp_vector[i],tmp_vector[i+1]));
    }
  }
  else {
    throw new GeneralException("Antecedant not valid (VARIABLE1:FUNCT1 VARIABLE2:FUNCT2)",__FILE__,__LINE__);
  }

  tmp_vector.resize(0);
  tmp_string = strtok(const_cast<char*>(consequent.c_str()),": \n");

  do {
    tmp_vector.push_back(string(tmp_string));
    tmp_string = strtok(NULL,": \n");
  }
  while (tmp_string != NULL);

  if (tmp_vector.size() %2 == 0) {
    for (int i = 0; i < tmp_vector.size(); i+=2) {
      m_consequent.push_back(make_pair(tmp_vector[i],tmp_vector[i+1]));
    }
  }
  else {
    throw new GeneralException("Consequent not valid (VARIABLE1:FUNCT1 VARIABLE2:FUNCT2)",__FILE__,__LINE__);
  }


  print_rule(cerr);

}

//////////////////////////////////////////////////////////////////////
// Destruction
//////////////////////////////////////////////////////////////////////
FuzzyRule::~FuzzyRule() {

}
//////////////////////////////////////////////////////////////////////
// Adding an antecedant to the rule
//////////////////////////////////////////////////////////////////////
void FuzzyRule::add_antecedant(const string &set_name, const string &funct_name) {

  m_antecedant.push_back(pair<string,string>(set_name,funct_name));

}
//////////////////////////////////////////////////////////////////////
// Adding a consequent to the rule
//////////////////////////////////////////////////////////////////////
void FuzzyRule::add_consequent(const string &set_name, const string &funct_name) {

  m_consequent.push_back(pair<string,string>(set_name,funct_name));	

}
//////////////////////////////////////////////////////////////////////
// Printing the rule on a standard stream
//////////////////////////////////////////////////////////////////////
void FuzzyRule::print_rule(ostream &out) {

  out<<"Rule #"<<m_rule_number<<" IF ";
  
  for (int i = 0; i < m_antecedant.size(); i++) {
    out<<m_antecedant[i].first<< " IS " << m_antecedant[i].second;
    if (i < m_antecedant.size() - 1) {
      out <<" AND ";
    }
  }
  
  out<<" THEN ";
  
  for (int j = 0; j < m_consequent.size(); j++) {
    out<<m_consequent[j].first<< " IS " << m_consequent[j].second;
    if (j < m_consequent.size() - 1) {
      out <<" AND ";
    }
  }
  
  out<<endl;
  
}
//////////////////////////////////////////////////////////////////////
// calculate
//////////////////////////////////////////////////////////////////////

void FuzzyRule::calculate(int output_id, int count, Buffer &out) {

  cerr<<"calculate"<<endl;

  //cloning ourself as a Object not a node!
  out[count] = ObjectRef(clone());

}

FuzzyRule* FuzzyRule::clone() {
  

  FuzzyRule *my_clone = new FuzzyRule(m_rule_number);

  for (int i = 0; i < m_antecedant.size(); i++) {
    my_clone->m_antecedant.push_back(m_antecedant[i]);
  }

  for (int i = 0; i < m_consequent.size(); i++) {
    my_clone->m_consequent.push_back(m_consequent[i]);
  }

  return my_clone;
}

