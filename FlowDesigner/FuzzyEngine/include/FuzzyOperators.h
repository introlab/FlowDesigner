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
