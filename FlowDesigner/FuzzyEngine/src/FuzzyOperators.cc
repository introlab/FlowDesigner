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

// FuzzySet.cc: implementation of the FuzzySet class.
//
//////////////////////////////////////////////////////////////////////
#include "FuzzyOperators.h"


///FuzzyRule and Vector<FuzzyRule> concatenation
ObjectRef RuleConcatRuleVect(ObjectRef x, ObjectRef y) {

  Vector<FuzzyRule> &vect = object_cast<Vector<FuzzyRule> >(y);
  Vector<FuzzyRule> *output = new Vector<FuzzyRule>;

  (*output).push_back(object_cast<FuzzyRule>(x));

  for (int i = 0; i < vect.size(); i++) {
    (*output).push_back(vect[i]);
  }

  return ObjectRef(output);

}
REGISTER_DOUBLE_VTABLE(concatVtable, RuleConcatRuleVect, FuzzyRule, Vector<FuzzyRule>);

///Vector<FuzzyRule> and FuzzyRule concatenation
ObjectRef RuleConcatVectRule(ObjectRef x, ObjectRef y) {

  Vector<FuzzyRule> &vect = object_cast<Vector<FuzzyRule> >(x);

  Vector<FuzzyRule> *output = new Vector<FuzzyRule>;

  for (int i = 0; i < vect.size(); i++) {
    (*output).push_back(vect[i]);
  }

  (*output).push_back(object_cast<FuzzyRule>(y));

  return ObjectRef(output);

}
REGISTER_DOUBLE_VTABLE(concatVtable, RuleConcatVectRule, Vector<FuzzyRule>, FuzzyRule);

///FuzzyRule and FuzzyRule concatenation
ObjectRef RuleConcatRuleRule(ObjectRef x, ObjectRef y) {

  Vector<FuzzyRule> *output = new Vector<FuzzyRule>;
  (*output).push_back(object_cast<FuzzyRule>(x));
  (*output).push_back(object_cast<FuzzyRule>(y));
  
  return ObjectRef(output);

}
REGISTER_DOUBLE_VTABLE(concatVtable, RuleConcatRuleRule, FuzzyRule, FuzzyRule);


///Vector<FuzzyRule> and Vector<FuzzyRule> concatenation
ObjectRef RuleConcatVectVect(ObjectRef x, ObjectRef y) {

  Vector<FuzzyRule> &vect1 = object_cast<Vector<FuzzyRule> >(x);
  Vector<FuzzyRule> &vect2 = object_cast<Vector<FuzzyRule> >(y);

  Vector<FuzzyRule> *output = new Vector<FuzzyRule>;

  for (int i = 0; i < vect1.size(); i++) {
    (*output).push_back(vect1[i]);
  }

  for (int i = 0; i < vect2.size(); i++) {
    (*output).push_back(vect2[i]);
  }

  return ObjectRef(output);

}
REGISTER_DOUBLE_VTABLE(concatVtable, RuleConcatVectVect, Vector<FuzzyRule>, Vector<FuzzyRule>);
