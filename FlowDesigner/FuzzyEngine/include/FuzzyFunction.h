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

// FuzzyFunction.h: interface for the FuzzyFunction class.
//
//////////////////////////////////////////////////////////////////////
#ifndef _FUZZY_FUNCTION_H_
#define _FUZZY_FUNCTION_H_

#include <string>
#include <vector>
#include "BufferedNode.h"

using namespace std;

class FuzzyFunction : public BufferedNode, public Object{

public:

	//accessor for the function name
	const string & get_name();
	
	//virtual destructor
	virtual ~FuzzyFunction();

	//pure virtual function
	virtual float evaluate(float x) = 0;

	//the constructor with a function name
	FuzzyFunction(const string &name);

	FuzzyFunction(string nodeName, ParameterSet params);


	//reset the inference vector
	void reset_inference_values() {m_inference_values.resize(0);}

	//return the inference vector
	vector<float> & get_inference_values() {return m_inference_values;}

	//push a value in the vector
	void push_inference_value(float value) {m_inference_values.push_back(value);}

	//computes the area (pure virtual function)
	virtual float get_area() = 0;

	//computes the center of gravity (pure virtual function)
	virtual float get_center_of_gravity() = 0;

	//lower limits of the function
	virtual float get_upper_bound() = 0;

	//higher limit of the functions
	virtual float get_lower_bound() = 0;

	//cloning capability
	virtual FuzzyFunction* clone() = 0;



protected:

	//the function name (linguistic term)
	string m_name;

	//the vector used for inference (temporary)
	vector<float> m_inference_values;

	int m_functionID;

private:

	//we shouldn't use this default constructor
	FuzzyFunction();

};

#endif
