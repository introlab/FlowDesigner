// Copyright (C) 2000 Dominic Letourneau (doumdi@yahoo.com)

// FuzzySet.cc: implementation of the FuzzySet class.
//
//////////////////////////////////////////////////////////////////////
#include "DoubleDispatch.h"
#include "FuzzyRule.h"
#include "FuzzySet.h"
#include "FuzzyFunction.h"
#include "Vector.h"

DEFINE_DOUBLE_VTABLE(concatVtable);

inline ObjectRef concatenate(ObjectRef x, ObjectRef y) {
   return concatVtable::perform(x,y);
}



inline istream& operator>> (istream &in, FuzzyRule *rule) {
  return in;
}

inline ostream& operator<< (ostream &out, FuzzyRule *rule) {
  return out;
}

inline istream& operator>> (istream &in, FuzzyRule &rule) {
  return in;
}

inline ostream& operator<< (ostream &out, FuzzyRule &rule) {
  return out;
}

inline istream& operator>> (istream &in, FuzzyFunction *fct) {
  return in;
}

inline ostream& operator<< (ostream &out, FuzzyFunction *fct) {
  return out;
}
inline istream& operator>> (istream &in, FuzzyFunction &fct) {
  return in;
}

inline ostream& operator<< (ostream &out, FuzzyFunction &fct) {
  return out;
}

inline istream& operator>> (istream &in, FuzzySet *set) {
  return in;
}

inline ostream& operator<< (ostream &out, FuzzySet *set) {
  return out;
}
inline istream& operator>> (istream &in, FuzzySet &set) {
  return in;
}

inline ostream& operator<< (ostream &out, FuzzySet &set) {
  return out;
}
