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

DECLARE_NODE(FuzzyRule)
/*Node
 *
 * @name FuzzyRule
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

FuzzyRule::FuzzyRule(int rule_number)
: m_rule_number(rule_number) {

}
FuzzyRule::FuzzyRule(string nodeName, ParameterSet params) 
: BufferedNode(nodeName,params) {
  

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



}
