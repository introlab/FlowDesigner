//Copyright (C) 2004 Dominic Letourneau (dominic.letourneau@usherbrooke.ca)

#ifndef _EQUAL_OPERATORS_CC_
#define _EQUAL_OPERATORS_CC_


#include "operators.h"
#include "net_types.h"
#include "Vector.h"
#include "Matrix.h"
#include "Complex.h"

//@implements core

template <class X, class Y, class Z>
ObjectRef equalCTypeFunction(ObjectRef op1, ObjectRef op2) {
  RCPtr<X> op1Value = op1;
  RCPtr<Y> op2Value = op2;
  RCPtr<Z> resultValue(Z::alloc(static_cast<typename Z::basicType>(op1Value->val()) == static_cast<typename Z::basicType>(op2Value->val())));
  return resultValue;
}
REGISTER_ALL_SCALAR_NO_COMPLEX_LOGIC_VTABLE(equalVtable, equalCTypeFunction);


#endif
