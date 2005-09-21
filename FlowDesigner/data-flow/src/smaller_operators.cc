//Copyright (C) 2004 Dominic Letourneau (dominic.letourneau@usherbrooke.ca)

#ifndef _SMALLER_OPERATORS_CC_
#define _SMALLER_OPERATORS_CC_

#include "operators.h"
#include "net_types.h"
#include "Vector.h"
#include "Matrix.h"
#include "Complex.h"

//@implements core

using namespace std;

namespace FD {

template <class X, class Y, class Z>
ObjectRef smallerCTypeFunction(ObjectRef op1, ObjectRef op2) {
  RCPtr<Z> op1Value = op1;
  RCPtr<Z> op2Value = op2;
  return ObjectRef(Bool::alloc(static_cast<typename Z::basicType>(op1Value->val()) < static_cast<typename Z::basicType>(op2Value->val())));  
}
REGISTER_ALL_SCALAR_NO_COMPLEX_VTABLE(smallerVtable, smallerCTypeFunction);


}//namespace FD

#endif
