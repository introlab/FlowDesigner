//Copyright (C) 2004 Dominic Letourneau (dominic.letourneau@usherbrooke.ca)

#ifndef _MAX_OPERATORS_CC_
#define _MAX_OPERATORS_CC_

#include "operators.h"
#include "net_types.h"
#include "Vector.h"
#include "Matrix.h"
#include "Complex.h"

//@implements core
using namespace std;
using namespace FD;

template <class X, class Y, class Z>
ObjectRef maxCTypeFunction(ObjectRef op1, ObjectRef op2) {
  RCPtr<X> op1Value = op1;
  RCPtr<Y> op2Value = op2;
  RCPtr<Z> resultValue(Z::alloc(max(static_cast<typename Z::basicType>(op1Value->val()),static_cast<typename Z::basicType>(op2Value->val()))));
  return resultValue;
}
REGISTER_ALL_SCALAR_NO_COMPLEX_VTABLE(maxVtable, maxCTypeFunction);


template<class X, class Y, class Z>
ObjectRef maxVectorFunction(ObjectRef op1, ObjectRef op2) {
  RCPtr<X> op1Value = op1;
  RCPtr<Y> op2Value = op2;

  if (op1Value->size() != op2Value->size()) {
    throw new GeneralException("MaxVectorFunction : Vector size mismatch ",__FILE__,__LINE__);
  }

  //creating new vector
  RCPtr<Z> resultValue(Z::alloc(op1Value->size()));

  for (int i = 0; i < resultValue->size(); i++) {    
    (*resultValue)[i] = max(static_cast<typename Z::basicType> ((*op1Value)[i]),static_cast<typename Z::basicType> ((*op2Value)[i]));
  }

  return resultValue;
}
REGISTER_ALL_VECTOR_NO_COMPLEX_VTABLE(maxVtable, maxVectorFunction);

template<class X, class Y, class Z>
ObjectRef maxMatrixFunction(ObjectRef op1, ObjectRef op2) {
  RCPtr<X> op1Value = op1;
  RCPtr<Y> op2Value = op2;
  
  if (op1Value->nrows() != op2Value->nrows() || op1Value->ncols() != op2Value->ncols()) {
    throw new GeneralException("MaxMatrixFunction : Matrix size mismatch ",__FILE__,__LINE__);
  }
  
  //creating new Matrix
  //TODO use Matrix pool?
  
  RCPtr<Z> resultValue(new Z(op1Value->nrows(), op1Value->ncols()));
  
  for (int i = 0; i < resultValue->nrows(); i++) {    
    for (int j = 0; j < resultValue->ncols(); j++) {      
      (*resultValue)(i,j) = max(static_cast<typename Z::basicType> ((*op1Value)(i,j)), static_cast<typename Z::basicType> ((*op2Value)(i,j)));    
     }
  }

  return resultValue;
}
REGISTER_ALL_MATRIX_NO_COMPLEX_VTABLE(maxVtable, maxMatrixFunction);

ObjectRef maxVectorObjectRef(ObjectRef op1, ObjectRef op2) {

  RCPtr<Vector<ObjectRef> > op1Value = op1;
  RCPtr<Vector<ObjectRef> > op2Value = op1;

  if (op1Value->size() != op2Value->size()) {
    throw new GeneralException("MaxVectorFunction : Vector size mismatch ",__FILE__,__LINE__);
  }

  //creating new vector
  RCPtr<Vector<ObjectRef> > resultValue(new Vector<ObjectRef>(op1Value->size()));

  for (int i = 0; i < resultValue->size(); i++) {    
    (*resultValue)[i] = max((*op1Value)[i], (*op2Value)[i]);
  }

  return resultValue;
}
REGISTER_DOUBLE_VTABLE(maxVtable,maxVectorObjectRef,Vector<ObjectRef>,Vector<ObjectRef>);

ObjectRef maxMatrixObjectRef(ObjectRef op1, ObjectRef op2) {

  RCPtr<Matrix<ObjectRef> > op1Value = op1;
  RCPtr<Matrix<ObjectRef> > op2Value = op1;

  if (op1Value->nrows() != op2Value->nrows() || op1Value->ncols() != op2Value->ncols()) {
    throw new GeneralException("MaxMatrixFunction : Matrix size mismatch ",__FILE__,__LINE__);
  }

  //creating new Matrix
  RCPtr<Matrix<ObjectRef> > resultValue(new Matrix<ObjectRef>(op1Value->nrows(), op1Value->ncols()));

  for (int i = 0; i < resultValue->nrows(); i++) {    
    for (int j = 0; j < resultValue->ncols(); j++) {      
      (*resultValue)(i,j) = max((*op1Value)(i,j),(*op2Value)(i,j));    
    }
  }
  return resultValue;
}
REGISTER_DOUBLE_VTABLE(maxVtable,maxMatrixObjectRef,Matrix<ObjectRef>,Matrix<ObjectRef>);

#endif
