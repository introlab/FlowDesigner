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

// TriangularFunction.h: interface for the TriangularFunction class.
//
//////////////////////////////////////////////////////////////////////
#ifndef _TRIANGULARFUNCTION_H_
#define _TRIANGULARFUNCTION_H_

#include "FuzzyFunction.h"
#include <string>

using namespace std;

class TriangularFunction : public FuzzyFunction  {

public:

	//constructor
	TriangularFunction(const string &name, float a, float b, float c);

	//destructor
	virtual ~TriangularFunction();

	//evaluation function
	virtual float evaluate(float index);

	//area evaluation
	virtual float get_area();

	//cog evaluation
	virtual float get_center_of_gravity();

	//lower limits of the function
	virtual float get_upper_bound() {return m_c;}

	//higher limit of the functions
	virtual float get_lower_bound() {return m_a;}


private:

	//function limits
	float m_a;
	float m_b;
	float m_c;

};

#endif 
