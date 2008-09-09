// Copyright (C) 2000 Dominic Letourneau (dominic.letourneau@courrier.usherb.ca)

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

namespace FD {

class FuzzyRule : public BufferedNode {

public:
	
  friend std::istream& operator>> (std::istream &in, FuzzyRule &rule);
  friend std::ostream& operator<< (std::ostream &out, FuzzyRule &rule);
  

  //printing the rule IF ... AND ... THEN ...
  void print_rule (std::ostream &out);

  //add a consequent in the rule
  void add_consequent (const std::string &set_name, const std::string &funct_name);
  
  //add an antecedant in the rule
  void add_antecedant(const std::string& set_name, const std::string &funct_name);
  
  
  FuzzyRule();
  
  //the constructor needs a rule number
  FuzzyRule(int rule_number);
  
  FuzzyRule(const FuzzyRule& cpy);
  
  FuzzyRule(std::string nodeName, ParameterSet params);

  FuzzyRule(std::istream &in);
  
  //destructor
  virtual ~FuzzyRule();
  
  //accessor for the antecedant part of the rule 
  std::vector<std::pair<std::string,std::string> >& get_antecedant_part() {return m_antecedant;}
  
  //accessor for the consequent of the rule
  std::vector<std::pair<std::string,std::string> >& get_consequent_part() {return m_consequent;}
  
  //accessor for the rule number
  int get_rule_number() {return m_rule_number;}

  void set_rule_number(int rule_number) {m_rule_number = rule_number;}
  
  virtual void calculate(int output_id, int count, Buffer &out);
    
  virtual ObjectRef clone();

  virtual void printOn(std::ostream &out=std::cout) const;

  virtual void readFrom(std::istream &in=std::cin);
  
 private:
  
  //vector of antecedant of type (FUZZY_SET,VARIABLE)
  std::vector<std::pair<std::string,std::string> > m_antecedant;
  
  //vector of consequent of type (FUZZY_SET,VARIABLE)
  std::vector<std::pair<std::string,std::string> > m_consequent;
  
  //the rule number	
  int m_rule_number;

  int m_ruleID;
};

template<>
inline void Vector<FuzzyRule*>::printOn(std::ostream &out) const {
  for (size_t i = 0; i < size(); i++) {
    operator[](i)->printOn(out);
  }
}

template<>
inline void Vector<FuzzyRule*>::destroy() {

 for (Vector<FuzzyRule*>::iterator iter = this->begin();
      iter != this->end(); iter++) {
   delete (*iter);
 }

 delete this;
}

}//namespace FD

#endif 
