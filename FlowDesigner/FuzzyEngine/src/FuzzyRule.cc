// FuzzyRule.cpp: implementation of the FuzzyRule class.
//
//////////////////////////////////////////////////////////////////////

#include "FuzzyRule.h"

//////////////////////////////////////////////////////////////////////
// Construction
//////////////////////////////////////////////////////////////////////

FuzzyRule::FuzzyRule(int rule_number)
: m_rule_number(rule_number) {

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
