// Copyright (C) 2000 Dominic Letourneau (doumdi@yahoo.com)

// FuzzyRule.cc: implementation of the FuzzyRule class.
//
//////////////////////////////////////////////////////////////////////

#include "FuzzyRule.h"
#include "Tokenizer.h"
#include <string>
#include "Vector.h"
#include "FuzzyOperators.h"


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
  : BufferedNode(nodeName,params), m_rule_number(-1) {
  
  cerr<<"FuzzyRule constructor"<<endl;

  m_ruleID = addOutput("RULE");
  String antecedant  = object_cast<String>(parameters.get("IF"));
  String consequent  = object_cast<String>(parameters.get("THEN"));

  if (antecedant.size() == 0 || consequent.size() == 0) {
    throw new GeneralException("Antecedant or consequent not specified",__FILE__,__LINE__);
  }

  vector<char> discardToken(3);
  vector<char> keepToken;
  discardToken[0]= ' ';
  discardToken[1]= ':';
  discardToken[2]= ';';


  vector<string> tokens;
  string_to_token(tokens,antecedant,keepToken,discardToken);


  if (tokens.size() %2 == 0 && tokens.size() != 0) { 
    for (int i = 0; i < tokens.size(); i+= 2) {
      m_antecedant.push_back(make_pair(tokens[i],tokens[i+1]));
    }
  }
  else {
    throw new GeneralException("Use VARIABLE1:VALUE VARIABLE2:VALUE as antecedant",__FILE__,__LINE__);
  }
   
  string_to_token(tokens,consequent,keepToken,discardToken);
  
  if (tokens.size() %2 == 0 && tokens.size() != 0) { 
    for (int i = 0; i < tokens.size(); i+=2) {
      m_consequent.push_back(make_pair(tokens[i],tokens[i+1]));
    }
  }
  else {
    throw new GeneralException("Use VARIABLE1:VALUE VARIABLE2:VALUE as consequent",__FILE__,__LINE__);
  }

  


  //print_rule(cerr);

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

  //cloning ourself as a Object not a node!
  out[count] = ObjectRef(new Vector<FuzzyRule*>(1,clone()));

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

