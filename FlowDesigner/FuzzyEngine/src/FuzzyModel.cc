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

// FuzzyModel.cc: implementation of the FuzzyModel class.
//
//////////////////////////////////////////////////////////////////////

#include "FuzzyModel.h"
#include "Exception.h"

//////////////////////////////////////////////////////////////////////
// Constants
//////////////////////////////////////////////////////////////////////
const int FuzzyModel::FUZZY_INPUT_SET = 0;
const int FuzzyModel::FUZZY_OUTPUT_SET = 1;

//////////////////////////////////////////////////////////////////////
// Construction
//////////////////////////////////////////////////////////////////////
FuzzyModel::FuzzyModel() {

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
	int rule_number = rule->get_rule_number();

	//let's verify this rule
	vector<pair<string,string> >&consequent = rule->get_consequent_part();
	vector<pair<string,string> >&antecedant = rule->get_antecedant_part();


	//rule number verification
	if (m_input_names.size() < rule_number) {
		m_input_names.resize(rule_number);
	}
	else {
		if (!m_input_names[rule_number -1].empty()) {
			char message[256];	
			sprintf(message,"RULE %i ALREADY EXISTS",rule_number);
			throw GeneralException(message,__FILE__,__LINE__);
		}
	}
	if (m_output_functions.size() < rule_number) {
		m_output_functions.resize(rule_number);
	}
	else {
		if (!m_output_functions[rule_number -1].empty()) {
			char message[256];	
			sprintf(message,"RULE %i ALREADY EXISTS",rule_number);
			throw GeneralException(message,__FILE__,__LINE__);
		}
	}


	//antecedant verification
	for (i = 0; i < antecedant.size(); i++) {

		set = find_set_named(antecedant[i].first, FuzzyModel::FUZZY_INPUT_SET);

		if (!set) {
			char message[256];
			sprintf(message,"SET NOT FOUND (%s)",antecedant[i].first.c_str());
			throw GeneralException(message,__FILE__,__LINE__);
		}
		else {
			function = set->find_function_by_name(antecedant[i].second);


			if (!function) {
				char message[256];
			    sprintf(message,"SET VARIABLE NOT FOUND (%s)(%s)",
					antecedant[i].first.c_str(),antecedant[i].second.c_str());
				throw GeneralException(message,__FILE__,__LINE__);
			}
			else {
				//keeping the name of the antecedant
				m_input_names[rule_number - 1].push_back(antecedant[i].second);				
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
			throw GeneralException(message,__FILE__,__LINE__);
		}
		else {
			function = set->find_function_by_name(consequent[i].second);


			if (!function) {
				char message[256];
			    sprintf(message,"SET VARIABLE NOT FOUND (%s)(%s)",
					consequent[i].first.c_str(),consequent[i].second.c_str());
				throw GeneralException(message,__FILE__,__LINE__);
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
		throw GeneralException("NULL SET",__FILE__,__LINE__);
	}

	switch (type) {
	case FUZZY_INPUT_SET:
		m_input_set.push_back(set);
		break;
	case FUZZY_OUTPUT_SET:
		m_output_set.push_back(set);
		break;
	default:
		throw GeneralException("UNKNOWN SET TYPE",__FILE__,__LINE__);
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
		throw GeneralException(message,__FILE__,__LINE__);
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
		throw GeneralException("UNKNOWN SET TYPE",__FILE__,__LINE__);
		break;
	}


	return NULL;
}
//////////////////////////////////////////////////////////////////////
// Evaluation/inference 
//////////////////////////////////////////////////////////////////////
vector<pair<string,float> >& FuzzyModel::evaluate(list<pair<string, float> > &input_values) {

	vector<float> inputs;

	if (input_values.size() != m_input_set.size()) {		
		throw GeneralException("NOT ENOUGH INPUT VARIABLES",__FILE__,__LINE__);	
	}



	//we must reset every output and input set
	int i;

	for (i = 0; i < m_input_set.size(); i++) {
		m_input_set[i]->reset();
	}

	for (i =0; i < m_output_set.size(); i++) {
		m_output_set[i]->reset();
	}


	for (list<pair<string,float> >::iterator iter = input_values.begin();
	iter != input_values.end(); iter++) {

		FuzzySet * set = find_set_named((*iter).first, FuzzyModel::FUZZY_INPUT_SET);
		
		if (!set) {
			char message[256];
			sprintf(message,"SET NOT FOUND (%s)",(*iter).first.c_str());
			throw GeneralException(message,__FILE__,__LINE__);		
		}
		else {
			inputs.push_back((*iter).second);

			//all membership evaluation
			set->get_all_membership_evaluation((*iter).second);
		}	
	}

	//we are assuming that the input variables are in the same order than the rules
	vector<float> conjunction_values(m_input_set.size());
	
	
	for (int x = 0; x < m_rules.size(); x++) {

		vector<string>::iterator iter_input;

		int y = 0;
			
		for (iter_input = m_input_names[x].begin();
		iter_input != m_input_names[x].end(); iter_input++) {
			
			conjunction_values[y] = m_input_set[y]->get_value_with_name((*iter_input));
			y++;
		}//for every antecedant of the rule

		list<FuzzyFunction*>::iterator iter;

		//applying conjunction operator
		for (iter = m_output_functions[x].begin();
		iter != m_output_functions[x].end(); iter++) {
			(*iter)->push_inference_value(conjunction(conjunction_values));			
		}

	}//for every rules


	//applying disjunction (once per output function)
	float disjunction_value;


	for (i = 0; i < m_output_set.size(); i++) {

	   vector<FuzzyFunction*> &funct = m_output_set[i]->get_member_functions();

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
