//Copyright (C) 2004 Dominic Letourneau (dominic.letourneau@usherbrooke.ca)

#ifndef _DIV_OPERATORS_CC_
#define _DIV_OPERATORS_CC_

#include "operators.h"
#include "net_types.h"
#include "Vector.h"
#include "Matrix.h"
#include "Complex.h"

//@implements core

template <class X, class Y, class Z>
ObjectRef divCTypeFunction(ObjectRef op1, ObjectRef op2) {
  RCPtr<X> op1Value = op1;
  RCPtr<Y> op2Value = op2;
  RCPtr<Z> resultValue(Z::alloc(static_cast<typename Z::basicType> (static_cast<typename Z::basicType>(op1Value->val()) / static_cast<typename Z::basicType>(op2Value->val()))));
  return resultValue;
}
REGISTER_ALL_SCALAR_VTABLE(divVtable, divCTypeFunction);

template<class X, class Y, class Z>
ObjectRef divVectorFunction(ObjectRef op1, ObjectRef op2) {
  RCPtr<X> op1Value = op1;
  RCPtr<Y> op2Value = op2;

  if (op1Value->size() != op2Value->size()) {
    throw new GeneralException("DivVectorFunction : Vector size mismatch ",__FILE__,__LINE__);
  }

  //creating new vector
  RCPtr<Z> resultValue(Z::alloc(op1Value->size()));

  for (int i = 0; i < resultValue->size(); i++) {    
    (*resultValue)[i] = static_cast<typename Z::basicType> ((*op1Value)[i]) / static_cast<typename Z::basicType> ((*op2Value)[i]);
  }

  return resultValue;
}
REGISTER_ALL_VECTOR_VTABLE(divVtable, divVectorFunction);

template<class X, class Y, class Z>
ObjectRef divMatrixFunction(ObjectRef op1, ObjectRef op2) {
  RCPtr<X> op1Value = op1;
  RCPtr<Y> op2Value = op2;
  
  if (op1Value->nrows() != op2Value->nrows() || op1Value->ncols() != op2Value->ncols()) {
    throw new GeneralException("DivMatrixFunction : Matrix size mismatch ",__FILE__,__LINE__);
  }
  
  //creating new Matrix
  //TODO use Matrix pool?
  
  RCPtr<Z> resultValue(new Z(op1Value->nrows(), op1Value->ncols()));
  
  for (int i = 0; i < resultValue->nrows(); i++) {    
    for (int j = 0; j < resultValue->ncols(); j++) {      
      (*resultValue)(i,j) = static_cast<typename Z::basicType> ((*op1Value)(i,j)) / static_cast<typename Z::basicType> ((*op2Value)(i,j));    
     }
  }

  return resultValue;
}
REGISTER_ALL_MATRIX_VTABLE(divVtable, divMatrixFunction);

#endif
