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

// FuzzyFunction.cc: implementation of the FuzzyFunction class.
//
//////////////////////////////////////////////////////////////////////

#include "FuzzyFunction.h"


//////////////////////////////////////////////////////////////////////
// Construction
//////////////////////////////////////////////////////////////////////

FuzzyFunction::FuzzyFunction() {

}

FuzzyFunction::FuzzyFunction(const string &name) 
: m_name(name) {


}

FuzzyFunction::FuzzyFunction(string nodeName, ParameterSet params)
 : BufferedNode(nodeName,params), m_name(nodeName) {

}
//////////////////////////////////////////////////////////////////////
// Destruction
//////////////////////////////////////////////////////////////////////
FuzzyFunction::~FuzzyFunction() {

}
//////////////////////////////////////////////////////////////////////
// returns the name of the function
//////////////////////////////////////////////////////////////////////
const string & FuzzyFunction::get_name() {
  return m_name;
}



