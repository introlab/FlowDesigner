// Copyright (C) 2000 Dominic Letourneau (doumdi@yahoo.com)

// FuzzySet.h: interface for the FuzzySet class.
//
//////////////////////////////////////////////////////////////////////
#ifndef _FUZZY_SET_H_
#define _FUZZY_SET_H_

#include <string>
#include <vector>
#include <map>
#include "FuzzyFunction.h"
#include "TrapezoidalFunction.h"
#include "TriangularFunction.h"
#include "BufferedNode.h"

using namespace std;

class FuzzySet : public BufferedNode, public Object {

public:
	
	//returns the index of the function in the set
	int find_function_by_index(const string &name);

	//returns the pointer of the function of a given name
	FuzzyFunction *find_function_by_name (const string &name);

	//adds a triangular function to the set
	void add_triangular_function (const string &name, float a, float b, float c);

	//adds a trapezoidal function to the set
	void add_trapezoidal_function (const string &name, float a, float b, float c, float d);

	//constructor with a name
	FuzzySet(const string &name);

	FuzzySet(string nodeName, ParameterSet params);

	FuzzySet();

	FuzzySet(istream &in) {readFrom(in);}

	//destructor
	virtual ~FuzzySet();

	//evaluation of all the membership function at once
	vector<float> & get_all_membership_evaluation(float x);

	//get the evaluation of the membership function of a given name
	float get_membership_evaluation(const string &name, float x);

	//accessor for the name
	const string& get_name(){return m_name;}

	//reset all functions inference stacks
	void reset() {for (int i = 0 ; i < m_functions.size(); i++) m_functions[i]->reset_inference_values();}
	
	//retuns the internal vector of fuzzy functions
	vector<FuzzyFunction*> & get_member_functions() {return m_functions;}

	//returns the number of function in the set
	int get_function_count() {return m_functions.size();}

	//print the membership functions
	void print_functions(ostream &out);

	virtual void calculate(int output_id, int count, Buffer &out);

	FuzzySet* clone();
	
	virtual void printOn(ostream &out);

	virtual void readFrom(istream &in=cin);

private:

	//the set name
	string m_name;

	//the vector of fuzzy functions
	vector<FuzzyFunction*> m_functions;

	//the evaluation vector (results of the evaluation of functions)
	vector<float> m_evaluation;

	//the inputID
	int m_functionID;

	//the outputID
	int m_setID;
};



inline void Vector<FuzzySet*>::printOn(ostream &out) const {

  cerr<<"PrintOn called (Vector<FuzzySet*>)"<<endl;
  
  for (int i = 0; i < size(); i++) {
    cerr<<"calling printon"<<endl;
    operator[](i)->printOn(out);
  }
}

inline void Vector<FuzzySet*>::readFrom(istream &in) {

}


inline void Vector<FuzzySet*>::destroy() {

 for (Vector<FuzzySet*>::iterator iter = this->begin();
      iter != this->end(); iter++) {
   delete (*iter);
 }

 delete this;
}

#endif
