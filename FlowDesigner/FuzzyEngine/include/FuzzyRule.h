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
