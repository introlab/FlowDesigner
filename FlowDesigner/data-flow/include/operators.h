#ifndef _OPERATORS_H_
#define _OPERATORS_H_

// Copyright (C) 1999 Jean-Marc Valin

#include "DoubleDispatch.h"
#include "misc.h"

namespace FD {

DEFINE_DOUBLE_VTABLE(addVtable);

inline ObjectRef operator+(ObjectRef x, ObjectRef y)
{
   return addVtable::perform(x,y);
}

DEFINE_DOUBLE_VTABLE(subVtable);

inline ObjectRef operator-(ObjectRef x, ObjectRef y)
{
   return subVtable::perform(x,y);
}

DEFINE_DOUBLE_VTABLE(mulVtable);

inline ObjectRef operator*(ObjectRef x, ObjectRef y)
{
   return mulVtable::perform(x,y);
}

DEFINE_DOUBLE_VTABLE(divVtable);

inline ObjectRef operator/(ObjectRef x, ObjectRef y)
{
   return divVtable::perform(x,y);
}

DEFINE_DOUBLE_VTABLE(smallerVtable);

inline ObjectRef operator<(ObjectRef x, ObjectRef y)
{
  return smallerVtable::perform(x,y);
}

inline ObjectRef operator>(ObjectRef x, ObjectRef y)
{
  return smallerVtable::perform(y,x);
}


DEFINE_DOUBLE_VTABLE(equalVtable);

inline ObjectRef operator==(ObjectRef x, ObjectRef y)
{
   return equalVtable::perform(x,y);
}


DEFINE_DOUBLE_VTABLE(maxVtable);
inline ObjectRef max(ObjectRef x, ObjectRef y)
{
   return maxVtable::perform(x,y);
}


DEFINE_DOUBLE_VTABLE(minVtable);
inline ObjectRef min(ObjectRef x, ObjectRef y)
{
   return minVtable::perform(x,y);
}


DEFINE_DOUBLE_VTABLE(concatVtable);
inline ObjectRef concat(ObjectRef x, ObjectRef y)
{
   return concatVtable::perform(x,y);
}

#define REGISTER_ALL_SCALAR_VTABLE(table, function)  \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Int,Int,Int,0)	  \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Int,Float,Float,1)	\
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Int,Double,Double,2) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Float,Int,Float,3) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Float,Float,Float,4) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Float,Double,Double,5) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Double,Int,Double,6) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Double,Float,Double,7) \
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

#define REGISTER_ALL_SCALAR_NO_COMPLEX_VTABLE(table, function)  \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Int,Int,Int,0)	  \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Int,Float,Float,1)	\
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Int,Double,Double,2) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Float,Int,Float,3) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Float,Float,Float,4) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Float,Double,Double,5) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Double,Int,Double,6) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Double,Float,Double,7) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Double,Double,Double,8) 

#define REGISTER_ALL_SCALAR_TO_VECTOR_VTABLE(table, function)  \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Int,Int,Vector<int>,0) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Int,Float,Vector<float>,1) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Int,Double,Vector<double>,2) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Float,Int,Vector<float>,3) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Float,Float,Vector<float>,4) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Float,Double,Vector<double>,5) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Double,Int,Vector<double>,6) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Double,Float,Vector<double>,7) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Double,Double,Vector<double>,8) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Int,Complex<float>,Vector<std::complex<float> >,9) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Int,Complex<double>,Vector<std::complex<double> >,10) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Complex<float>,Int,Vector<std::complex<float> >,11) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Complex<double>,Int,Vector<std::complex<double> >,12) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Float,Complex<float>,Vector<std::complex<float> >,13) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Float,Complex<double>,Vector<std::complex<double> >,14) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Complex<float>,Float,Vector<std::complex<float> >,15) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Complex<double>,Float,Vector<std::complex<double> >,16) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Double,Complex<float>,Vector<std::complex<double> >,17) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Double,Complex<double>,Vector<std::complex<double> >,18) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Complex<float>,Double,Vector<std::complex<double> >,19) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Complex<float>,Complex<float>,Vector<std::complex<float> >,20) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Complex<float>,Complex<double>,Vector<std::complex<double> >,21) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Complex<double>,Complex<float>,Vector<std::complex<double> >,22) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Complex<double>,Complex<double>,Vector<std::complex<double> >,23)


#define REGISTER_ALL_VECTOR_VTABLE(table,function) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<int>,Vector<int>,Vector<int>,0) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<int>,Vector<float>,Vector<float>,1) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<int>,Vector<double>,Vector<double>,2) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<int>,Vector<std::complex<float> >,Vector<std::complex<float> >,3) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<int>,Vector<std::complex<double> >,Vector<std::complex<double> >,4) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<float>,Vector<int>,Vector<float>,5) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<float>,Vector<float>,Vector<float>,6) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<float>,Vector<double>,Vector<double>,7) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<float>,Vector<std::complex<float> >,Vector<std::complex<float> >,8) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<float>,Vector<std::complex<double> >,Vector<std::complex<double> >,9) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<double>,Vector<int>,Vector<double>,10) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<double>,Vector<float>,Vector<double>,11) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<double>,Vector<double>,Vector<double>,12) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<double>,Vector<std::complex<float> >,Vector<std::complex<double> >,13) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<double>,Vector<std::complex<double> >,Vector<std::complex<double> >,14) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<std::complex<float> >,Vector<int>,Vector<std::complex<float> >,15) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<std::complex<float> >,Vector<float>,Vector<std::complex<float> >,16) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<std::complex<float> >,Vector<double>,Vector<std::complex<double> >,17) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<std::complex<double> >,Vector<int>,Vector<std::complex<double> >,18) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<std::complex<double> >,Vector<float>,Vector<std::complex<double> >,19) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<std::complex<double> >,Vector<double>,Vector<std::complex<double> >,20) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<std::complex<float> >,Vector<std::complex<float> >,Vector<std::complex<float> >,21) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<std::complex<float> >,Vector<std::complex<double> >,Vector<std::complex<double> >,22) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<std::complex<double> >,Vector<std::complex<float> >,Vector<std::complex<double> >,23) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<std::complex<double> >,Vector<std::complex<double> >,Vector<std::complex<double> >,24)


#define REGISTER_ALL_VECTOR_NO_COMPLEX_VTABLE(table,function) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<int>,Vector<int>,Vector<int>,0) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<int>,Vector<float>,Vector<float>,1) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<int>,Vector<double>,Vector<double>,2) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<float>,Vector<int>,Vector<float>,3) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<float>,Vector<float>,Vector<float>,4) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<float>,Vector<double>,Vector<double>,5) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<double>,Vector<int>,Vector<double>,6) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<double>,Vector<float>,Vector<double>,7) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<double>,Vector<double>,Vector<double>,8)


#define REGISTER_ALL_SCALAR_VECTOR_VTABLE(table,function) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Int,Vector<int>,Vector<int>,0) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Int,Vector<float>,Vector<float>,1) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Int,Vector<double>,Vector<double>,2) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Int,Vector<std::complex<float> >,Vector<std::complex<float> >,3) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Int,Vector<std::complex<double> >,Vector<std::complex<double> >,4) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Float,Vector<int>,Vector<float>,5) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Float,Vector<float>,Vector<float>,6) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Float,Vector<double>,Vector<double>,7) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Float,Vector<std::complex<float> >,Vector<std::complex<float> >,8) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Float,Vector<std::complex<double> >,Vector<std::complex<double> >,9) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Double,Vector<int>,Vector<double>,10) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Double,Vector<float>,Vector<double>,11) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Double,Vector<double>,Vector<double>,12) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Double,Vector<std::complex<float> >,Vector<std::complex<double> >,13) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Double,Vector<std::complex<double> >,Vector<std::complex<double> >,14) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Complex<float>,Vector<int>,Vector<std::complex<float> >,15) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Complex<float>,Vector<float>,Vector<std::complex<float> >,16) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Complex<float>,Vector<double>,Vector<std::complex<double> >,17) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Complex<float>,Vector<std::complex<float> >,Vector<std::complex<float> >,18) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Complex<float>,Vector<std::complex<double> >,Vector<std::complex<double> >,19) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Complex<double>,Vector<int>,Vector<std::complex<double> >,20) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Complex<double>,Vector<float>,Vector<std::complex<double> >,21) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Complex<double>,Vector<double>,Vector<std::complex<double> >,22) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Complex<double>,Vector<std::complex<float> >,Vector<std::complex<double> >,23) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Complex<double>,Vector<std::complex<double> >,Vector<std::complex<double> >,24) 


#define REGISTER_ALL_VECTOR_SCALAR_VTABLE(table,function) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<int>,Int,Vector<int>,0) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<float>,Int,Vector<float>,1) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<double>,Int,Vector<double>,2) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<std::complex<float> >,Int,Vector<std::complex<float> >,3) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<std::complex<double> >,Int,Vector<std::complex<double> >,4) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<int>,Float,Vector<float>,5) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<float>,Float,Vector<float>,6) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<double>,Float,Vector<double>,7) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<std::complex<float> >,Float,Vector<std::complex<float> >,8) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<std::complex<double> >,Float,Vector<std::complex<double> >,9) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<int>,Double,Vector<double>,10) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<float>,Double,Vector<double>,11) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<double>,Double,Vector<double>,12) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<std::complex<float> >,Double,Vector<std::complex<double> >,13) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<std::complex<double> >,Double,Vector<std::complex<double> >,14) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<int>,Complex<float>,Vector<std::complex<float> >,15) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<float>,Complex<float>,Vector<std::complex<float> >,16) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<double>,Complex<float>,Vector<std::complex<double> >,17) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<std::complex<float> >,Complex<float>,Vector<std::complex<float> >,18) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<std::complex<double> >,Complex<float>,Vector<std::complex<double> >,19) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<int>,Complex<double>,Vector<std::complex<double> >,20) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<float>,Complex<double>,Vector<std::complex<double> >,21) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<double>,Complex<double>,Vector<std::complex<double> >,22) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<std::complex<float> >,Complex<double>,Vector<std::complex<double> >,23) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<std::complex<double> >,Complex<double>,Vector<std::complex<double> >,24) 


#define REGISTER_ALL_MATRIX_VTABLE(table,function) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Matrix<int>,Matrix<int>,Matrix<int>,0) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Matrix<int>,Matrix<float>,Matrix<float>,1) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Matrix<int>,Matrix<double>,Matrix<double>,2) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Matrix<int>,Matrix<std::complex<float> >,Matrix<std::complex<float> >,3) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Matrix<int>,Matrix<std::complex<double> >,Matrix<std::complex<double> >,4) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Matrix<float>,Matrix<int>,Matrix<float>,5) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Matrix<float>,Matrix<float>,Matrix<float>,6) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Matrix<float>,Matrix<double>,Matrix<double>,7) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Matrix<float>,Matrix<std::complex<float> >,Matrix<std::complex<float> >,8) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Matrix<float>,Matrix<std::complex<double> >,Matrix<std::complex<double> >,9) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Matrix<double>,Matrix<int>,Matrix<double>,10) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Matrix<double>,Matrix<float>,Matrix<double>,11) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Matrix<double>,Matrix<double>,Matrix<double>,12) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Matrix<double>,Matrix<std::complex<float> >,Matrix<std::complex<double> >,13) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Matrix<double>,Matrix<std::complex<double> >,Matrix<std::complex<double> >,14) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Matrix<std::complex<float> >,Matrix<int>,Matrix<std::complex<float> >,15) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Matrix<std::complex<float> >,Matrix<float>,Matrix<std::complex<float> >,16) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Matrix<std::complex<float> >,Matrix<double>,Matrix<std::complex<double> >,17) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Matrix<std::complex<double> >,Matrix<int>,Matrix<std::complex<double> >,18) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Matrix<std::complex<double> >,Matrix<float>,Matrix<std::complex<double> >,19) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Matrix<std::complex<double> >,Matrix<double>,Matrix<std::complex<double> >,20) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Matrix<std::complex<float> >,Matrix<std::complex<float> >,Matrix<std::complex<float> >,21) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Matrix<std::complex<float> >,Matrix<std::complex<double> >,Matrix<std::complex<double> >,22) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Matrix<std::complex<double> >,Matrix<std::complex<float> >,Matrix<std::complex<double> >,23) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Matrix<std::complex<double> >,Matrix<std::complex<double> >,Matrix<std::complex<double> >,24)


#define REGISTER_ALL_MATRIX_NO_COMPLEX_VTABLE(table,function) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Matrix<int>,Matrix<int>,Matrix<int>,0) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Matrix<int>,Matrix<float>,Matrix<float>,1) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Matrix<int>,Matrix<double>,Matrix<double>,2) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Matrix<float>,Matrix<int>,Matrix<float>,3) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Matrix<float>,Matrix<float>,Matrix<float>,4) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Matrix<float>,Matrix<double>,Matrix<double>,5) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Matrix<double>,Matrix<int>,Matrix<double>,6) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Matrix<double>,Matrix<float>,Matrix<double>,7) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Matrix<double>,Matrix<double>,Matrix<double>,8)

#define REGISTER_ALL_SCALAR_MATRIX_VTABLE(table,function) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Int,Matrix<int>,Matrix<int>,0) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Int,Matrix<float>,Matrix<float>,1) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Int,Matrix<double>,Matrix<double>,2) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Int,Matrix<std::complex<float> >,Matrix<std::complex<float> >,3) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Int,Matrix<std::complex<double> >,Matrix<std::complex<double> >,4) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Float,Matrix<int>,Matrix<float>,5) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Float,Matrix<float>,Matrix<float>,6) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Float,Matrix<double>,Matrix<double>,7) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Float,Matrix<std::complex<float> >,Matrix<std::complex<float> >,8) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Float,Matrix<std::complex<double> >,Matrix<std::complex<double> >,9) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Double,Matrix<int>,Matrix<double>,10) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Double,Matrix<float>,Matrix<double>,11) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Double,Matrix<double>,Matrix<double>,12) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Double,Matrix<std::complex<float> >,Matrix<std::complex<double> >,13) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Double,Matrix<std::complex<double> >,Matrix<std::complex<double> >,14) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Complex<float>,Matrix<int>,Matrix<std::complex<float> >,15) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Complex<float>,Matrix<float>,Matrix<std::complex<float> >,16) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Complex<float>,Matrix<double>,Matrix<std::complex<double> >,17) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Complex<float>,Matrix<std::complex<float> >,Matrix<std::complex<float> >,18) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Complex<float>,Matrix<std::complex<double> >,Matrix<std::complex<double> >,19) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Complex<double>,Matrix<int>,Matrix<std::complex<double> >,20) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Complex<double>,Matrix<float>,Matrix<std::complex<double> >,21) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Complex<double>,Matrix<double>,Matrix<std::complex<double> >,22) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Complex<double>,Matrix<std::complex<float> >,Matrix<std::complex<double> >,23) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Complex<double>,Matrix<std::complex<double> >,Matrix<std::complex<double> >,24) 


#define REGISTER_ALL_MATRIX_SCALAR_VTABLE(table,function) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Matrix<int>,Int,Matrix<int>,0) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Matrix<float>,Int,Matrix<float>,1) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Matrix<double>,Int,Matrix<double>,2) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Matrix<std::complex<float> >,Int,Matrix<std::complex<float> >,3) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Matrix<std::complex<double> >,Int,Matrix<std::complex<double> >,4) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Matrix<int>,Float,Matrix<float>,5) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Matrix<float>,Float,Matrix<float>,6) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Matrix<double>,Float,Matrix<double>,7) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Matrix<std::complex<float> >,Float,Matrix<std::complex<float> >,8) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Matrix<std::complex<double> >,Float,Matrix<std::complex<double> >,9) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Matrix<int>,Double,Matrix<double>,10) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Matrix<float>,Double,Matrix<double>,11) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Matrix<double>,Double,Matrix<double>,12) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Matrix<std::complex<float> >,Double,Matrix<std::complex<double> >,13) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Matrix<std::complex<double> >,Double,Matrix<std::complex<double> >,14) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Matrix<int>,Complex<float>,Matrix<std::complex<float> >,15) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Matrix<float>,Complex<float>,Matrix<std::complex<float> >,16) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Matrix<double>,Complex<float>,Matrix<std::complex<double> >,17) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Matrix<std::complex<float> >,Complex<float>,Matrix<std::complex<float> >,18) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Matrix<std::complex<double> >,Complex<float>,Matrix<std::complex<double> >,19) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Matrix<int>,Complex<double>,Matrix<std::complex<double> >,20) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Matrix<float>,Complex<double>,Matrix<std::complex<double> >,21) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Matrix<double>,Complex<double>,Matrix<std::complex<double> >,22) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Matrix<std::complex<float> >,Complex<double>,Matrix<std::complex<double> >,23) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Matrix<std::complex<double> >,Complex<double>,Matrix<std::complex<double> >,24) 

}//namespace FD

#endif
