//Copyright (C) 2004 Dominic Letourneau (dominic.letourneau@usherbrooke.ca)

#ifndef _CONCAT_OPERATORS_CC_
#define _CONCAT_OPERATORS_CC_

#include "operators.h"
#include "net_types.h"
#include "Vector.h"
#include "Matrix.h"
#include "Complex.h"

//@implements core

template <class X, class Y, class Z>
ObjectRef concatCTypeFunction(ObjectRef op1, ObjectRef op2) {

  RCPtr<X> op1Value = op1;
  RCPtr<Y> op2Value = op2;
  
  //creating output vector
  RCPtr<Z> resultValue = RCPtr<Z>(Z::alloc(2));
  
  //concat 2 values into a vector
  (*resultValue)[0] =  static_cast<typename Z::basicType>(op1Value->val());
  (*resultValue)[1] =  static_cast<typename Z::basicType>(op2Value->val());

  return resultValue;
}
REGISTER_ALL_SCALAR_TO_VECTOR_VTABLE(concatVtable, concatCTypeFunction);


template <class X, class Y, class Z>
ObjectRef concatVectorFunction(ObjectRef op1, ObjectRef op2) {

  RCPtr<X> op1Value = op1;
  RCPtr<Y> op2Value = op2;

  RCPtr<Z> resultValue(Z::alloc(op1Value->size() + op2Value->size()));
  
  //copy first part
  for (int i =0; i < op1Value->size(); i++) {
    (*resultValue)[i] = static_cast<typename Z::basicType>((*op1Value)[i]);
  }
  //copy last part
  for (int i =0; i < op2Value->size(); i++) {
    (*resultValue)[i + op1Value->size()] = static_cast<typename Z::basicType>((*op2Value)[i]);
  }
  return resultValue;
}
REGISTER_ALL_VECTOR_VTABLE(concatVtable, concatVectorFunction);

template<class X, class Y, class Z>
ObjectRef concatVectorScalarFunction(ObjectRef op1, ObjectRef op2) {
  RCPtr<X> op1Value = op1;
  RCPtr<Y> op2Value = op2;

  //creating new vector
  RCPtr<Z> resultValue(Z::alloc(op1Value->size() + 1));

  //copying values from vector
  for (int i = 0; i < resultValue->size(); i++) {    
    (*resultValue)[i] = static_cast<typename Z::basicType> ((*op1Value)[i]);
  }

  //adding last element
  (*resultValue)[resultValue->size() - 1] = static_cast<typename Z::basicType>(op2Value->val());

  return resultValue;
}
REGISTER_ALL_VECTOR_SCALAR_VTABLE(concatVtable, concatVectorScalarFunction);


template<class X, class Y, class Z>
ObjectRef concatScalarVectorFunction(ObjectRef op1, ObjectRef op2) {
  RCPtr<X> op1Value = op1;
  RCPtr<Y> op2Value = op2;

  //creating new vector
  RCPtr<Z> resultValue(Z::alloc(op2Value->size() + 1));

  //copying values from vector
  for (int i = 1; i < resultValue->size(); i++) {    
    (*resultValue)[i] = static_cast<typename Z::basicType> ((*op2Value)[i - 1]);
  }

  //adding first element
  (*resultValue)[0] = static_cast<typename Z::basicType>(op1Value->val());

  return resultValue;
}
REGISTER_ALL_SCALAR_VECTOR_VTABLE(concatVtable, concatScalarVectorFunction);


//TODO Matrix stuff?


ObjectRef concatVectorObjectRef(ObjectRef op1, ObjectRef op2) {

  RCPtr<Vector<ObjectRef> > op1Value = op1;
  RCPtr<Vector<ObjectRef> > op2Value = op1;

  //creating new vector
  RCPtr<Vector<ObjectRef> > resultValue(new Vector<ObjectRef>(op1Value->size() + op2Value->size()));

  //copying first part of the vector
  for (int i = 0; i < op1Value->size(); i++) {        
    (*resultValue)[i] = (*op1Value)[i]->clone();
  }
  
  //copying last part of the vector
  for (int i = op1Value->size(); i < resultValue->size(); i++) {    
    (*resultValue)[i] = (*op2Value)[i - op1Value->size()]->clone();
  }

  return resultValue;
}
REGISTER_DOUBLE_VTABLE(concatVtable,concatVectorObjectRef,Vector<ObjectRef>,Vector<ObjectRef>);




#endif
