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

// GenericModel.h: interface for the GenericModel class.
//
//////////////////////////////////////////////////////////////////////

#ifndef _GENERIC_MODEL_H_
#define _GENERIC_MODEL_H_

#include "FuzzyModel.h"

class GenericModel : public FuzzyModel  {

public:

	//constructor
	GenericModel();
	GenericModel(string nodeName, ParameterSet params);

	//destructor
	virtual ~GenericModel();

	//fuzzy conjunction operator (min)
	virtual float conjunction(vector<float> &c_values);

	//fuzzy disjunction operator (max)
	virtual float disjunction(vector<float> &d_values);

	//Mamdani defuzzification 
	virtual vector<pair<string,float> > & defuzzification();

	virtual void calculate(int output_id, int count, Buffer &out);
	

private:

};

#endif 
