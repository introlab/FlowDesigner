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

// GenericModel.cc: implementation of the GenericModel class.
//
//////////////////////////////////////////////////////////////////////

#include "GenericModel.h"
#include "Exception.h"
//////////////////////////////////////////////////////////////////////
// Destruction
//////////////////////////////////////////////////////////////////////

GenericModel::GenericModel()
{

}
//////////////////////////////////////////////////////////////////////
// Destruction
//////////////////////////////////////////////////////////////////////
GenericModel::~GenericModel()
{

}

//////////////////////////////////////////////////////////////////////
// Fuzzy conjunction operator (min)
//////////////////////////////////////////////////////////////////////
float GenericModel::conjunction(vector<float> &c_values) {


	if (c_values.empty()) {
		throw GeneralException("Conjunction values vector empty",__FILE__,__LINE__);
	}

	float value = c_values[0];

	//min operator
	for (int i = 0; i < c_values.size(); i++) {
		if (c_values[i] < value) {
			value = c_values[i];
		}
	}


	return value;
}
//////////////////////////////////////////////////////////////////////
// Fuzzy disjunction operator (max)
//////////////////////////////////////////////////////////////////////
float GenericModel::disjunction(vector<float> &d_values) {

	if (d_values.empty()) {
		throw GeneralException("Disjunction values vector empty",__FILE__,__LINE__);
	}

	float value = d_values[0];

	//max operator
	for (int i = 0; i < d_values.size(); i++) {

		if (d_values[i] > value) {
			value = d_values[i];
		}
	}

	return value;
}


vector<pair<string,float> > & GenericModel::defuzzification() {

	m_defuzzification.resize(0);

	float defuzz_value;
	float area_cumul;
	float tmp_area;
	float tmp_cog;

	//for every output set we will calculate the defuzzified value
	//using Larsen (product) and the centroid method


	for (int i = 0; i < m_output_set.size(); i++) {

		area_cumul = 0;
		defuzz_value = 0;

		vector<FuzzyFunction*> & functions = m_output_set[i]->get_member_functions();

		for (int j = 0;j < functions.size(); j++) {
			
			tmp_area = functions[j]->get_area();
			tmp_cog = functions[j]->get_center_of_gravity();
			area_cumul += tmp_area;
			defuzz_value += tmp_area * tmp_cog;
		}//for every member function

		if (area_cumul != 0) {
			defuzz_value /= area_cumul;
		}
		//adding this output
		m_defuzzification.push_back(pair<string,float>(m_output_set[i]->get_name(),defuzz_value));

	}//for every set


	return m_defuzzification;
}
