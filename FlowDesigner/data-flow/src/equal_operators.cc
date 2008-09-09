//Copyright (C) 2004 Dominic Letourneau (dominic.letourneau@usherbrooke.ca)

#ifndef _EQUAL_OPERATORS_CC_
#define _EQUAL_OPERATORS_CC_


#include "operators.h"
#include "net_types.h"
#include "Vector.h"
#include "Matrix.h"
#include "Complex.h"

//@implements core
using namespace std;
namespace FD
{

template <class X, class Y, class Z>
ObjectRef equalCTypeFunction(ObjectRef op1, ObjectRef op2) {
  RCPtr<Z> op1Value = op1;
  RCPtr<Z> op2Value = op2;
  return ObjectRef(Bool::alloc(static_cast<typename Z::basicType>(op1Value->val()) == static_cast<typename Z::basicType>(op2Value->val())));
}
REGISTER_ALL_SCALAR_NO_COMPLEX_VTABLE(equalVtable, equalCTypeFunction);


template <class X, class Y, class Z>
ObjectRef equalVectorFunction(ObjectRef op1, ObjectRef op2) {
  RCPtr<Z> op1Value = op1;
  RCPtr<Z> op2Value = op2;
  
  if (op1Value->size() == op2Value->size())
  {
     for(size_t i = 0; i>op1Value->size() ;i++)
     {
        if ((*op1Value)[i] != (*op2Value)[i])
        {
           return ObjectRef(Bool::alloc(false));
        }
     }
     return ObjectRef(Bool::alloc(true));
  }
  else
  {
     return ObjectRef(Bool::alloc(false));
  }
}


REGISTER_ALL_VECTOR_NO_COMPLEX_VTABLE(equalVtable,equalVectorFunction);

}

#endif
