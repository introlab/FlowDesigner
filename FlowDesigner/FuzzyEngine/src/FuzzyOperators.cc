// Copyright (C) 2000 Dominic Letourneau (doumdi@yahoo.com)

// FuzzySet.cc: implementation of the FuzzySet class.
//
//////////////////////////////////////////////////////////////////////
#include "FuzzyOperators.h"

///Vector<ObjectRef> and Vector<ObjectRef> concatenation
ObjectRef ObjectRefConcatVectVect(ObjectRef x, ObjectRef y) {

  Vector<ObjectRef> &vect1 = object_cast<Vector<ObjectRef> >(x);
  Vector<ObjectRef> &vect2 = object_cast<Vector<ObjectRef> >(y);

  Vector<ObjectRef> *output = new Vector<ObjectRef>;

  for (int i = 0; i < vect1.size(); i++) {
    (*output).push_back(vect1[i]->clone());
  }

  for (int i = 0; i < vect2.size(); i++) {
    (*output).push_back(vect2[i]->clone());
  }

  return ObjectRef(output);

}
REGISTER_DOUBLE_VTABLE(concatVtable, ObjectRefConcatVectVect, Vector<ObjectRef>, Vector<ObjectRef>);


/*

///Vector<FuzzyFunction*> and Vector<FuzzyFunction*> concatenation
ObjectRef RuleConcatVectVect(ObjectRef x, ObjectRef y) {

  Vector<FuzzyRule*> &vect1 = object_cast<Vector<FuzzyRule*> >(x);
  Vector<FuzzyRule*> &vect2 = object_cast<Vector<FuzzyRule*> >(y);

  Vector<FuzzyRule*> *output = new Vector<FuzzyRule*>;

  for (int i = 0; i < vect1.size(); i++) {
    (*output).push_back(vect1[i]->clone());
  }

  for (int i = 0; i < vect2.size(); i++) {
    (*output).push_back(vect2[i]->clone());
  }

  return ObjectRef(output);

}
REGISTER_DOUBLE_VTABLE(concatVtable, RuleConcatVectVect, Vector<FuzzyRule*>, Vector<FuzzyRule*>);

///Vector<FuzzyFunction*> and Vector<FuzzyFunction*> concatenation
ObjectRef FunctionConcatVectVect(ObjectRef x, ObjectRef y) {

  Vector<FuzzyFunction*> &vect1 = object_cast<Vector<FuzzyFunction*> >(x);
  Vector<FuzzyFunction*> &vect2 = object_cast<Vector<FuzzyFunction*> >(y);

  Vector<FuzzyFunction*> *output = new Vector<FuzzyFunction*>;

  for (int i = 0; i < vect1.size(); i++) {
    (*output).push_back(vect1[i]->clone());
  }

  for (int i = 0; i < vect2.size(); i++) {
    (*output).push_back(vect2[i]->clone());
  }

  return ObjectRef(output);

}
REGISTER_DOUBLE_VTABLE(concatVtable, FunctionConcatVectVect, Vector<FuzzyFunction*>, Vector<FuzzyFunction*>);


///Vector<FuzzySet*> and Vector<FuzzySet*> concatenation
ObjectRef SetConcatVectVect(ObjectRef x, ObjectRef y) {

  Vector<FuzzySet*> &vect1 = object_cast<Vector<FuzzySet*> >(x);
  Vector<FuzzySet*> &vect2 = object_cast<Vector<FuzzySet*> >(y);

  Vector<FuzzySet*> *output = new Vector<FuzzySet*>;

  for (int i = 0; i < vect1.size(); i++) {
    (*output).push_back(vect1[i]->clone());
  }

  for (int i = 0; i < vect2.size(); i++) {
    (*output).push_back(vect2[i]->clone());
  }

  return ObjectRef(output);

}
REGISTER_DOUBLE_VTABLE(concatVtable, SetConcatVectVect, Vector<FuzzySet*>, Vector<FuzzySet*>);

*/


