// Copyright (C) 2000 Dominic Letourneau (doumdi@yahoo.com)

// FuzzyRule.h: interface for the FuzzyRule class.
//
//////////////////////////////////////////////////////////////////////
#ifndef _FUZZYRULE_H_
#define _FUZZYRULE_H_

#include <string>
#include <vector>
#include "BufferedNode.h"
#include "Object.h"
#include "Vector.h"

using namespace std;

class FuzzyRule : public BufferedNode, public Object {

public:
	
  friend istream& operator>> (istream &in, FuzzyRule &rule);
  friend ostream& operator<< (ostream &out, FuzzyRule &rule);
  

  //printing the rule IF ... AND ... THEN ...
  void print_rule (ostream &out);

  //add a consequent in the rule
  void add_consequent (const string &set_name, const string &funct_name);
  
  //add an antecedant in the rule
  void add_antecedant(const string& set_name, const string &funct_name);
  
  
  FuzzyRule();
  
  //the constructor needs a rule number
  FuzzyRule(int rule_number);
  
  FuzzyRule(const FuzzyRule& cpy);
  
  FuzzyRule(string nodeName, ParameterSet params);
  
  //destructor
  virtual ~FuzzyRule();
  
  //accessor for the antecedant part of the rule 
  vector<pair<string,string> >& get_antecedant_part() {return m_antecedant;}
  
  //accessor for the consequent of the rule
  vector<pair<string,string> >& get_consequent_part() {return m_consequent;}
  
  //accessor for the rule number
  int get_rule_number() {return m_rule_number;}

  void set_rule_number(int rule_number) {m_rule_number = rule_number;}
  
  virtual void calculate(int output_id, int count, Buffer &out);
  
  
  FuzzyRule* clone();

  virtual void printOn(ostream &out=cout) const;


  
 private:
  
  //vector of antecedant of type (FUZZY_SET,VARIABLE)
  vector<pair<string,string> > m_antecedant;
  
  //vector of consequent of type (FUZZY_SET,VARIABLE)
  vector<pair<string,string> > m_consequent;
  
  //the rule number	
  int m_rule_number;

  int m_ruleID;
};

inline void Vector<FuzzyRule*>::printOn(ostream &out) const {
  for (int i = 0; i < size(); i++) {
    operator[](i)->printOn(out);
  }
}

inline void Vector<FuzzyRule*>::destroy() {

 for (Vector<FuzzyRule*>::iterator iter = this->begin();
      iter != this->end(); iter++) {
   delete (*iter);
 }

 delete this;
}

#endif 
