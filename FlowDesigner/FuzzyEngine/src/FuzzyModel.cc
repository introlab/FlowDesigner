// Copyright (C) 2000 Dominic Letourneau (doumdi@yahoo.com)

// FuzzyModel.cc: implementation of the FuzzyModel class.
//
//////////////////////////////////////////////////////////////////////

#include "FuzzyModel.h"
#include "Exception.h"
#include <stdlib.h>
#include <stdio.h>
#include <string>
#include <iostream>

//////////////////////////////////////////////////////////////////////
// Constants
//////////////////////////////////////////////////////////////////////
const int FuzzyModel::FUZZY_INPUT_SET = 0;
const int FuzzyModel::FUZZY_OUTPUT_SET = 1;

//////////////////////////////////////////////////////////////////////
// Construction
//////////////////////////////////////////////////////////////////////
FuzzyModel::FuzzyModel()
  : BufferedNode("INVALID",ParameterSet()) {

}

FuzzyModel::FuzzyModel(const FuzzyModel &model)
  : BufferedNode("INVALID",ParameterSet()) {

  //inserting input set
  //cerr<<"copying input set"<<endl;
  for (int i = 0; i < model.m_input_set.size(); i++) {
    m_input_set.push_back(model.m_input_set[i]->clone());
  }

  //inserting output set
  //cerr<<"copying output set"<<endl;
  for (int i = 0; i < model.m_output_set.size(); i++) {
    m_output_set.push_back(model.m_output_set[i]->clone());
  }
  
  //readding rules
  //cerr<<"copying rules"<<endl;
  for (int i = 0; i < model.m_rules.size(); i++) {
    add_fuzzy_rule(model.m_rules[i]->clone());
  }

}


FuzzyModel::FuzzyModel(string nodeName, ParameterSet params)
  : BufferedNode(nodeName,params) {

  m_RuleID = addInput("RULES");;
  m_ASetID = addInput("ANTECEDENT_SETS");
  m_CSetID = addInput("CONSEQUENT_SETS");
  m_InputID = addInput("INPUT");
  m_OutputID = addOutput("OUTPUT");
  m_ModelID = addOutput("MODEL");

}
//////////////////////////////////////////////////////////////////////
// Destruction
//////////////////////////////////////////////////////////////////////
FuzzyModel::~FuzzyModel() {

  int i;
  
  //deleting rules
  for (i = 0; i < m_rules.size(); i++) {
    delete m_rules[i];
  }
  
  //deleting input sets
  for (i = 0; i <m_input_set.size(); i++) {
    delete m_input_set[i];
  }
  
  //deleting output sets
  for (i = 0; i <m_output_set.size(); i++) {
    delete m_output_set[i];
  }
  
  //resizing vectors
  m_rules.resize(0);
  m_input_set.resize(0);
  m_output_set.resize(0);
  
}
//////////////////////////////////////////////////////////////////////
// Add a fuzzy rule to the model
//////////////////////////////////////////////////////////////////////
void FuzzyModel::add_fuzzy_rule(FuzzyRule *rule) {


  
  FuzzySet *set;
  FuzzyFunction *function;
  int i;
  
  //int rule_number = rule->get_rule_number();
  int rule_number = m_rules.size() + 1;

  rule->set_rule_number(rule_number);
  
  //rule->print_rule(cerr);


  //let's verify this rule
  vector<pair<string,string> >&consequent = rule->get_consequent_part();
  vector<pair<string,string> >&antecedant = rule->get_antecedant_part();
  
  
  //rule number verification
  if (m_input_functions.size() < rule_number) {
    m_input_functions.resize(rule_number);
  }
  else {
    if (!m_input_functions[rule_number -1].empty()) {
      char message[256];	
      sprintf(message,"RULE %i ALREADY EXISTS",rule_number);
      throw new GeneralException(message,__FILE__,__LINE__);
    }
  }

  if (m_output_functions.size() < rule_number) {
    m_output_functions.resize(rule_number);
  }
  else {
    if (!m_output_functions[rule_number -1].empty()) {
      char message[256];	
      sprintf(message,"RULE %i ALREADY EXISTS",rule_number);
      throw new GeneralException(message,__FILE__,__LINE__);
    }
  }
  
  
  //antecedant verification
  for (i = 0; i < antecedant.size(); i++) {
    
    set = find_set_named(antecedant[i].first, FuzzyModel::FUZZY_INPUT_SET);
    
    if (!set) {
      char message[256];
      sprintf(message,"SET NOT FOUND (%s)",antecedant[i].first.c_str());
      throw new GeneralException(message,__FILE__,__LINE__);
    }
    else {
      function = set->find_function_by_name(antecedant[i].second);
      
      
      if (!function) {
	char message[256];
	sprintf(message,"SET VARIABLE NOT FOUND (%s)(%s)",
		antecedant[i].first.c_str(),antecedant[i].second.c_str());
	throw new GeneralException(message,__FILE__,__LINE__);
      }
      else {
	//keeping the name of the antecedant
	m_input_functions[rule_number -1].push_back(function);
      }
      
    }
  }
  //cout<<"antecedant verification OK"<<endl;
  
  
  //consequent verification
  for (i = 0; i < consequent.size(); i++) {
    
    set = find_set_named(consequent[i].first, FuzzyModel::FUZZY_OUTPUT_SET);
    
    if (!set) {
      char message[256];
      sprintf(message,"SET NOT FOUND (%s)",consequent[i].first.c_str());
      throw new GeneralException(message,__FILE__,__LINE__);
    }
    else {
      function = set->find_function_by_name(consequent[i].second);
      
      
      if (!function) {
	char message[256];
	sprintf(message,"SET VARIABLE NOT FOUND (%s)(%s)",
		consequent[i].first.c_str(),consequent[i].second.c_str());
	throw new GeneralException(message,__FILE__,__LINE__);
      }
      else {
	//keeping a pointer to this function for rule evaluation
	m_output_functions[rule_number - 1].push_back(function);
      }
      
    }
  }
  //cout<<"consequent verification OK"<<endl;
  
  
  //this rule is ready to be inserted
  m_rules.push_back(rule);
  
}
//////////////////////////////////////////////////////////////////////
// Add a fuzzy set to the model 
//////////////////////////////////////////////////////////////////////
void FuzzyModel::add_fuzzy_set(FuzzySet *set, int type) {
  
  
  if (!set) {
    throw new GeneralException("NULL SET",__FILE__,__LINE__);
  }
  
  switch (type) {
  case FUZZY_INPUT_SET:
    m_input_set.push_back(set);
    break;
  case FUZZY_OUTPUT_SET:
    m_output_set.push_back(set);
    break;
  default:
    throw new GeneralException("UNKNOWN SET TYPE",__FILE__,__LINE__);
    break;
    
  }
}
//////////////////////////////////////////////////////////////////////
// Print all the rules
//////////////////////////////////////////////////////////////////////
void FuzzyModel::print_rules(ostream &out) {
  
  for (int i = 0; i < m_rules.size(); i++) {
    m_rules[i]->print_rule(out);
  }
  
}
//////////////////////////////////////////////////////////////////////
// verification of rule consistency
//////////////////////////////////////////////////////////////////////
void FuzzyModel::verify_rules() {
  
  
  //verification of the number of rules
  //and the antecedant/consequent part of the rules
  //to be implemented 
  int count = 1;
  
  for (int i = 0; i < m_input_set.size(); i++) {
    count *= m_input_set[i]->get_function_count();
  }
  
  if (count != m_rules.size()) {
    char message[256];
    sprintf(message,"NUMBER OF RULES INCORRECT %i INSTEAD OF %i",m_rules.size(),count);
    throw new GeneralException(message,__FILE__,__LINE__);
  }
  
  
}
//////////////////////////////////////////////////////////////////////
// Find a set with a given name
//////////////////////////////////////////////////////////////////////
FuzzySet* FuzzyModel::find_set_named(const string &name, int type) {
  
  
  int i;
  
  switch(type) {
  case FUZZY_INPUT_SET:
    for (i =0; i < m_input_set.size(); i++) {
      if (m_input_set[i]->get_name() == name) {
	return m_input_set[i];
      }
    }
    
    break;
    
  case FUZZY_OUTPUT_SET:
    
    for (i =0; i < m_output_set.size(); i++) {
      if (m_output_set[i]->get_name() == name) {
	return m_output_set[i];
      }
    }
    
    break;
    
  default:
    throw new GeneralException("UNKNOWN SET TYPE",__FILE__,__LINE__);
    break;
  }
  
  
  return NULL;
}
//////////////////////////////////////////////////////////////////////
// Evaluation/inference 
//////////////////////////////////////////////////////////////////////
Vector<float>& FuzzyModel::evaluate(Vector<float>  &input_values) {
  
  Vector<float> inputs;
  
  if (input_values.size() != m_input_set.size()) {		
    throw new GeneralException("NOT ENOUGH INPUT VARIABLES",__FILE__,__LINE__);	
  }
    
  //we must reset every output and input set
  int i;
  
  for (i = 0; i < m_input_set.size(); i++) {
    m_input_set[i]->reset();
  }
  
  for (i =0; i < m_output_set.size(); i++) {
    m_output_set[i]->reset();
  }
  
  //assuming that values are ordered like the rules
  for (int i = 0; i < m_input_set.size(); i++) {
    inputs.push_back(input_values[i]);
    //m_input_set[i]->get_all_membership_evaluation(input_values[i]);
  }

  //we are assuming that the input variables are in the same order than the rules
  Vector<float> conjunction_values(m_input_set.size());
  
  
  for (int x = 0; x < m_rules.size(); x++) {
    
    list<FuzzyFunction*>::iterator iter_input;
    
    int y = 0;
    
    for (iter_input = m_input_functions[x].begin();
	 iter_input != m_input_functions[x].end(); iter_input++) {

      //conjunction_values[y] = m_input_set[y]->get_value_with_name((*iter_input));
      conjunction_values[y] = (*iter_input)->evaluate(inputs[y]);

      /*
	cerr<<"input set "<<m_input_set[y]->get_name()<<endl;
	cerr<<"input name "<<(*iter_input)->get_name()<<endl;
	cerr<<"getting conjunction_value "<<conjunction_values[y]<<endl;
      */
      
      y++;

    }//for every antecedant of the rule
    
    list<FuzzyFunction*>::iterator iter;
    
    //applying conjunction operator
    for (iter = m_output_functions[x].begin();
	 iter != m_output_functions[x].end(); iter++) {
      
      /*
	cerr<<"Pushing inference value of "<<conjunction(conjunction_values)
	<<" for output function "<<(*iter)->get_name()<<endl;
      */

      (*iter)->push_inference_value(conjunction(conjunction_values));			
    }
    
  }//for every rules
  
  
  //applying disjunction (once per output function)
  float disjunction_value;
  
  
  for (i = 0; i < m_output_set.size(); i++) {
    
    Vector<FuzzyFunction*> &funct = m_output_set[i]->get_member_functions();
    
    for (int j = 0; j < funct.size(); j++) {
      disjunction_value = disjunction(funct[j]->get_inference_values());
      funct[j]->reset_inference_values();
      funct[j]->push_inference_value(disjunction_value);
      /*
	printf("Final value for (%s,%s) : %f\n",m_output_set[i]->get_name().c_str(),
	funct[j]->get_name().c_str(),disjunction_value);
      */
    }
    
  }
  
  //defuzzification
  return defuzzification();
}
//////////////////////////////////////////////////////////////////////
// Printing the membership functions
//////////////////////////////////////////////////////////////////////
void FuzzyModel::print_sets(ostream &out) {
  
  out<<"INPUT SETS"<<endl;
  
  for (int i = 0; i < m_input_set.size(); i++) {
    out<<m_input_set[i]->get_name()<<endl;
    m_input_set[i]->print_functions(out);
  }
  
  
  out<<"OUTPUT SETS"<<endl;
  
  for (int j = 0; j < m_output_set.size(); j++) {
    out<<m_output_set[j]->get_name()<<endl;
    m_output_set[j]->print_functions(out);
  }
  
  
}


void FuzzyModel::reset() {

  for (int i = 0 ; i < m_rules.size(); i++) {
    delete m_rules[i];
  }
  m_rules.resize(0);


  for (int i = 0 ; i < m_output_set.size(); i++) {
    delete m_output_set[i];
  }
  m_output_set.resize(0);

  
  for (int i = 0 ; i < m_input_set.size(); i++) {
    delete m_input_set[i];
  }
  m_input_set.resize(0);
  
  m_input_functions.resize(0);
  m_output_functions.resize(0);

}
//////////////////////////////////////////////////////////////////////
// calculate
//////////////////////////////////////////////////////////////////////

void FuzzyModel::calculate(int output_id, int count, Buffer &out) {

  try {

    reset();
        
    //getting Fuzzy Rules
    ObjectRef Rules = getInput(m_RuleID, count);
    
    //getting Fuzzy Sets (antecedent)
    ObjectRef ASets = getInput(m_ASetID, count);
    
    //getting Fuzzy Sets (consequent)
    ObjectRef CSets = getInput(m_CSetID,count);
    
    //getting Inputs
    ObjectRef Input = getInput(m_InputID, count);
        
    //First add antecedant sets
    Vector<FuzzySet*> &vect_sets1 = object_cast<Vector<FuzzySet*> >(ASets);
    for (int i = 0; i < vect_sets1.size(); i++) {
      //vect_sets1[i]->printOn(cerr);
      add_fuzzy_set(vect_sets1[i]->clone(),FuzzyModel::FUZZY_INPUT_SET);
    }
    
    //Then add consequent sets
    Vector<FuzzySet*> &vect_sets2 = object_cast<Vector<FuzzySet*> >(CSets);
    for (int i = 0; i < vect_sets2.size(); i++) {
      //vect_sets2[i]->printOn(cerr);
      add_fuzzy_set(vect_sets2[i]->clone(),FuzzyModel::FUZZY_OUTPUT_SET);
    }
    
    //Finally add rules
    Vector<FuzzyRule*> &vect_rules = object_cast<Vector<FuzzyRule*> >(Rules);
    for (int i = 0; i < vect_rules.size(); i++) {
      //vect_rules[i]->printOn(cerr);
      add_fuzzy_rule(vect_rules[i]->clone());
    }
    
    //verify rule consistency
    verify_rules();
    
    //calculate output
    Vector<float> &vect_value = object_cast<Vector<float> >(Input);  

    Vector<float>& calc_output = evaluate(vect_value);
    
    Vector<float> *my_output = new Vector<float>(calc_output.size());
    
    for (int i = 0; i < calc_output.size(); i++) {
      (*my_output)[i] = calc_output[i];
    }
    
    if (output_id == m_OutputID) {
      out[count] = ObjectRef(my_output);
    }
    
    if (output_id == m_ModelID) {
      out[count] = ObjectRef(clone());
    }
  }//try
  catch (BaseException *e) {
    throw e->add (new GeneralException("Exception caught while processing GeneralModel", __FILE__, __LINE__));
  }


}
