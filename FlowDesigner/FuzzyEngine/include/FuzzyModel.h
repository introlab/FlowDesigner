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

// FuzzyModel.h: interface for the FuzzyModel class.
//
//////////////////////////////////////////////////////////////////////
#ifndef _FUZZY_MODEL_H_
#define _FUZZY_MODEL_H_


#include "FuzzySet.h"
#include "FuzzyRule.h"
#include <list>
#include <string>
#include "BufferedNode.h"

using namespace std;

class FuzzyModel : public BufferedNode {

public:
	
	//rules consistency verification
	void verify_rules();

	//print the rules
	void print_rules(ostream &out);

	//print the sets
	void print_sets(ostream &out);


	//add a fuzzy set (of type input or output)
	void add_fuzzy_set(FuzzySet *set, int type);

	//add a fuzzy rule (rules must be inserted after sets
	void add_fuzzy_rule (FuzzyRule *rule);

	//default constructor
	FuzzyModel();
	FuzzyModel(string nodeName, ParameterSet params);

	//destructor
	virtual ~FuzzyModel();

	//evaluate the model with the named input values (in rule order)
	virtual vector<pair<string,float> >& evaluate(list<pair<string, float> > &input_values);

	//pure virtual function that must be implemented by the
	//real model (conjunction,disjunction,defuzzification)
	virtual float conjunction(vector<float> &c_values) = 0;
	virtual float disjunction(vector<float> &d_values) = 0;
	virtual vector<pair<string,float> > & defuzzification() = 0;

	//constants representing input and output sets
	static const int FUZZY_INPUT_SET;
	static const int FUZZY_OUTPUT_SET;


protected:

	//vector of rules
	vector<FuzzyRule*> m_rules;

	//vector of input sets
	vector<FuzzySet*> m_input_set;

	//vector of output sets
	vector<FuzzySet*> m_output_set;

	//vector of input functions to be evaluated for a rule
	vector<vector<string> > m_input_names;

	//vector of output functions to be evaluated for a rule
	vector<list<FuzzyFunction*> > m_output_functions;

	//internal function to get a named set
	FuzzySet* find_set_named(const string &name, int type);

	//defuzzification result
	vector<pair<string,float> >m_defuzzification;

};

#endif