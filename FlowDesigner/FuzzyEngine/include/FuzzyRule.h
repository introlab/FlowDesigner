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

// FuzzyRule.h: interface for the FuzzyRule class.
//
//////////////////////////////////////////////////////////////////////
#ifndef _FUZZYRULE_H_
#define _FUZZYRULE_H_

#include <string>
#include <vector>

using namespace std;

class FuzzyRule  {

public:
	
	//printing the rule IF ... AND ... THEN ...
	void print_rule (ostream &out);

	//add a consequent in the rule
	void add_consequent (const string &set_name, const string &funct_name);

	//add an antecedant in the rule
	void add_antecedant(const string& set_name, const string &funct_name);

	//the constructor needs a rule number
	FuzzyRule(int rule_number);
	
	//destructor
	virtual ~FuzzyRule();
	
	//accessor for the antecedant part of the rule 
	vector<pair<string,string> >& get_antecedant_part() {return m_antecedant;}

	//accessor for the consequent of the rule
	vector<pair<string,string> >& get_consequent_part() {return m_consequent;}

	//accessor for the rule number
	int get_rule_number() {return m_rule_number;}


private:

	//vector of antecedant of type (FUZZY_SET,VARIABLE)
	vector<pair<string,string> > m_antecedant;
	
	//vector of consequent of type (FUZZY_SET,VARIABLE)
	vector<pair<string,string> > m_consequent;

	//the rule number	
	int m_rule_number;
};

#endif 
