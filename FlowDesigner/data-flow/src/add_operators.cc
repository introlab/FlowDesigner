#ifndef _ADD_OPERATORS_CC_
#define _ADD_OPERATORS_CC_

#include "operators.h"
#include "net_types.h"
#include "Vector.h"
#include "Matrix.h"

//@implements core

template <class X, class Y, class Z>
ObjectRef addCTypeFunction(ObjectRef op1, ObjectRef op2) {
  RCPtr<X> op1Value = op1;
  RCPtr<Y> op2Value = op2;
  RCPtr<Z> resultValue(Z::alloc(static_cast<typename Z::basicType> (op1Value->val() + op2Value->val())));
  return resultValue;
}

//Standard type add
REGISTER_DOUBLE_VTABLE_TEMPLATE(addVtable,addCTypeFunction,Int,Int,Int,0);
REGISTER_DOUBLE_VTABLE_TEMPLATE(addVtable,addCTypeFunction,Int,Float,Float,1);
REGISTER_DOUBLE_VTABLE_TEMPLATE(addVtable,addCTypeFunction,Int,Double,Double,2);

REGISTER_DOUBLE_VTABLE_TEMPLATE(addVtable,addCTypeFunction,Float,Int,Int,3);
REGISTER_DOUBLE_VTABLE_TEMPLATE(addVtable,addCTypeFunction,Float,Float,Float,4);
REGISTER_DOUBLE_VTABLE_TEMPLATE(addVtable,addCTypeFunction,Float,Double,Double,5);

REGISTER_DOUBLE_VTABLE_TEMPLATE(addVtable,addCTypeFunction,Double,Int,Int,6);
REGISTER_DOUBLE_VTABLE_TEMPLATE(addVtable,addCTypeFunction,Double,Float,Float,7);
REGISTER_DOUBLE_VTABLE_TEMPLATE(addVtable,addCTypeFunction,Double,Double,Double,8);


template<class X, class Y, class Z>
ObjectRef addVectorFunction(ObjectRef op1, ObjectRef op2) {
  RCPtr<X> op1Value = op1;
  RCPtr<Y> op2Value = op2;

  if (op1Value->size() != op2Value->size()) {
    throw new GeneralException("AddVectorFunction : Vector size mismatch ",__FILE__,__LINE__);
  }

  //creating new vector
  RCPtr<Z> resultValue(Z::alloc(op1Value->size()));

  for (int i = 0; i < resultValue->size(); i++) {
    (*resultValue)[i] = (*op1Value)[i] + (*op2Value)[i];
  }

  return resultValue;
}






#endif
