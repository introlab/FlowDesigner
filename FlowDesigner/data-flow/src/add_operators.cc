#ifndef _ADD_OPERATORS_CC_
#define _ADD_OPERATORS_CC_

#include "operators.h"
#include "net_types.h"
#include "Vector.h"
#include "Matrix.h"
#include "Complex.h"

//@implements core

#define REGISTER_ALL_SCALAR_VTABLE(table, function)  \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Int,Int,Int,0)	  \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Int,Float,Float,1)	\
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Int,Double,Double,2) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Float,Int,Int,3) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Float,Float,Float,4) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Float,Double,Double,5) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Double,Int,Int,6) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Double,Float,Float,7) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Double,Double,Double,8) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Int,Complex<float>,Complex<float>,9) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Int,Complex<double>,Complex<double>,10) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Complex<float>,Int,Complex<float>,11) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Complex<double>,Int,Complex<double>,12) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Float,Complex<float>,Complex<float>,13) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Float,Complex<double>,Complex<double>,14) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Complex<float>,Float,Complex<float>,15) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Complex<double>,Float,Complex<double>,16) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Double,Complex<float>,Complex<double>,17) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Double,Complex<double>,Complex<double>,18) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Complex<float>,Double,Complex<double>,19) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Complex<float>,Complex<float>,Complex<float>,20) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Complex<float>,Complex<double>,Complex<double>,21) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Complex<double>,Complex<float>,Complex<double>,22) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Complex<double>,Complex<double>,Complex<double>,23)

#define REGISTER_ALL_VECTOR_VTABLE(table,function) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<int>,Vector<int>,Vector<int>,0) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<int>,Vector<float>,Vector<float>,1) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<int>,Vector<double>,Vector<double>,2) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<float>,Vector<int>,Vector<float>,3) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<float>,Vector<float>,Vector<float>,4) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<float>,Vector<double>,Vector<double>,5) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<double>,Vector<int>,Vector<double>,6) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<double>,Vector<float>,Vector<double>,7) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<double>,Vector<double>,Vector<double>,8)

/*
 \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<complex<float> >,Vector<int>,Vector<complex<float> >,9) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<complex<float> >,Vector<float>,Vector<complex<float> >,10) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<complex<float> >,Vector<double>,Vector<complex<double> >,11) 
*/

template <class X, class Y, class Z>
ObjectRef addCTypeFunction(ObjectRef op1, ObjectRef op2) {
  RCPtr<X> op1Value = op1;
  RCPtr<Y> op2Value = op2;
  RCPtr<Z> resultValue(Z::alloc(static_cast<typename Z::basicType> (static_cast<typename Z::basicType>(op1Value->val()) + static_cast<typename Z::basicType>(op2Value->val()))));
  return resultValue;
}
REGISTER_ALL_SCALAR_VTABLE(addVtable, addCTypeFunction);

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
REGISTER_ALL_VECTOR_VTABLE(addVtable, addVectorFunction);





#endif
