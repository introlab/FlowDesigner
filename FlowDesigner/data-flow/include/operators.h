// Copyright (C) 1999 Jean-Marc Valin

#include "DoubleDispatch.h"
#include "misc.h"

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

#define REGISTER_ALL_SCALAR_NO_COMPLEX_VTABLE(table, function)  \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Int,Int,Int,0)	  \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Int,Float,Float,1)	\
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Int,Double,Double,2) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Float,Int,Int,3) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Float,Float,Float,4) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Float,Double,Double,5) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Double,Int,Int,6) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Double,Float,Float,7) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Double,Double,Double,8) 

#define REGISTER_ALL_SCALAR_NO_COMPLEX_LOGIC_VTABLE(table, function)  \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Int,Int,Bool,0)	  \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Int,Float,Bool,1)	\
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Int,Double,Bool,2) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Float,Int,Bool,3) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Float,Float,Bool,4) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Float,Double,Bool,5) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Double,Int,Bool,6) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Double,Float,Bool,7) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Double,Double,Bool,8) 


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
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Int,Complex<float>,Vector<complex<float> >,9) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Int,Complex<double>,Vector<complex<double> >,10) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Complex<float>,Int,Vector<complex<float> >,11) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Complex<double>,Int,Vector<complex<double> >,12) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Float,Complex<float>,Vector<complex<float> >,13) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Float,Complex<double>,Vector<complex<double> >,14) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Complex<float>,Float,Vector<complex<float> >,15) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Complex<double>,Float,Vector<complex<double> >,16) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Double,Complex<float>,Vector<complex<double> >,17) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Double,Complex<double>,Vector<complex<double> >,18) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Complex<float>,Double,Vector<complex<double> >,19) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Complex<float>,Complex<float>,Vector<complex<float> >,20) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Complex<float>,Complex<double>,Vector<complex<double> >,21) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Complex<double>,Complex<float>,Vector<complex<double> >,22) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Complex<double>,Complex<double>,Vector<complex<double> >,23)


#define REGISTER_ALL_VECTOR_VTABLE(table,function) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<int>,Vector<int>,Vector<int>,0) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<int>,Vector<float>,Vector<float>,1) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<int>,Vector<double>,Vector<double>,2) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<int>,Vector<complex<float> >,Vector<complex<float> >,3) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<int>,Vector<complex<double> >,Vector<complex<double> >,4) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<float>,Vector<int>,Vector<float>,5) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<float>,Vector<float>,Vector<float>,6) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<float>,Vector<double>,Vector<double>,7) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<float>,Vector<complex<float> >,Vector<complex<float> >,8) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<float>,Vector<complex<double> >,Vector<complex<double> >,9) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<double>,Vector<int>,Vector<double>,10) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<double>,Vector<float>,Vector<double>,11) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<double>,Vector<double>,Vector<double>,12) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<double>,Vector<complex<float> >,Vector<complex<double> >,13) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<double>,Vector<complex<double> >,Vector<complex<double> >,14) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<complex<float> >,Vector<int>,Vector<complex<float> >,15) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<complex<float> >,Vector<float>,Vector<complex<float> >,16) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<complex<float> >,Vector<double>,Vector<complex<double> >,17) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<complex<double> >,Vector<int>,Vector<complex<double> >,18) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<complex<double> >,Vector<float>,Vector<complex<double> >,19) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<complex<double> >,Vector<double>,Vector<complex<double> >,20) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<complex<float> >,Vector<complex<float> >,Vector<complex<float> >,21) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<complex<float> >,Vector<complex<double> >,Vector<complex<double> >,22) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<complex<double> >,Vector<complex<float> >,Vector<complex<double> >,23) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<complex<double> >,Vector<complex<double> >,Vector<complex<double> >,24)


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
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Int,Vector<complex<float> >,Vector<complex<float> >,3) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Int,Vector<complex<double> >,Vector<complex<double> >,4) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Float,Vector<int>,Vector<float>,5) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Float,Vector<float>,Vector<float>,6) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Float,Vector<double>,Vector<double>,7) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Float,Vector<complex<float> >,Vector<complex<float> >,8) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Float,Vector<complex<double> >,Vector<complex<double> >,9) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Double,Vector<int>,Vector<double>,10) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Double,Vector<float>,Vector<double>,11) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Double,Vector<double>,Vector<double>,12) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Double,Vector<complex<float> >,Vector<complex<double> >,13) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Double,Vector<complex<double> >,Vector<complex<double> >,14) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Complex<float>,Vector<int>,Vector<complex<float> >,15) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Complex<float>,Vector<float>,Vector<complex<float> >,16) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Complex<float>,Vector<double>,Vector<complex<double> >,17) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Complex<float>,Vector<complex<float> >,Vector<complex<float> >,18) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Complex<float>,Vector<complex<double> >,Vector<complex<double> >,19) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Complex<double>,Vector<int>,Vector<complex<double> >,20) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Complex<double>,Vector<float>,Vector<complex<double> >,21) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Complex<double>,Vector<double>,Vector<complex<double> >,22) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Complex<double>,Vector<complex<float> >,Vector<complex<double> >,23) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Complex<double>,Vector<complex<double> >,Vector<complex<double> >,24) 


#define REGISTER_ALL_VECTOR_SCALAR_VTABLE(table,function) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<int>,Int,Vector<int>,0) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<float>,Int,Vector<float>,1) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<double>,Int,Vector<double>,2) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<complex<float> >,Int,Vector<complex<float> >,3) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<complex<double> >,Int,Vector<complex<double> >,4) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<int>,Float,Vector<float>,5) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<float>,Float,Vector<float>,6) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<double>,Float,Vector<double>,7) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<complex<float> >,Float,Vector<complex<float> >,8) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<complex<double> >,Float,Vector<complex<double> >,9) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<int>,Double,Vector<double>,10) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<float>,Double,Vector<double>,11) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<double>,Double,Vector<double>,12) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<complex<float> >,Double,Vector<complex<double> >,13) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<complex<double> >,Double,Vector<complex<double> >,14) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<int>,Complex<float>,Vector<complex<float> >,15) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<float>,Complex<float>,Vector<complex<float> >,16) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<double>,Complex<float>,Vector<complex<double> >,17) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<complex<float> >,Complex<float>,Vector<complex<float> >,18) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<complex<double> >,Complex<float>,Vector<complex<double> >,19) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<int>,Complex<double>,Vector<complex<double> >,20) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<float>,Complex<double>,Vector<complex<double> >,21) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<double>,Complex<double>,Vector<complex<double> >,22) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<complex<float> >,Complex<double>,Vector<complex<double> >,23) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Vector<complex<double> >,Complex<double>,Vector<complex<double> >,24) 


#define REGISTER_ALL_MATRIX_VTABLE(table,function) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Matrix<int>,Matrix<int>,Matrix<int>,0) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Matrix<int>,Matrix<float>,Matrix<float>,1) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Matrix<int>,Matrix<double>,Matrix<double>,2) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Matrix<float>,Matrix<int>,Matrix<float>,3) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Matrix<float>,Matrix<float>,Matrix<float>,4) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Matrix<float>,Matrix<double>,Matrix<double>,5) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Matrix<double>,Matrix<int>,Matrix<double>,6) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Matrix<double>,Matrix<float>,Matrix<double>,7) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Matrix<double>,Matrix<double>,Matrix<double>,8) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Matrix<complex<float> >,Matrix<int>,Matrix<complex<float> >,9) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Matrix<complex<float> >,Matrix<float>,Matrix<complex<float> >,10) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Matrix<complex<float> >,Matrix<double>,Matrix<complex<double> >,11) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Matrix<complex<double> >,Matrix<int>,Matrix<complex<double> >,12) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Matrix<complex<double> >,Matrix<float>,Matrix<complex<double> >,13) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Matrix<complex<double> >,Matrix<double>,Matrix<complex<double> >,14) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Matrix<complex<float> >,Matrix<complex<float> >,Matrix<complex<float> >,15) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Matrix<complex<float> >,Matrix<complex<double> >,Matrix<complex<double> >,16) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Matrix<complex<double> >,Matrix<complex<float> >,Matrix<complex<double> >,17) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Matrix<complex<double> >,Matrix<complex<double> >,Matrix<complex<double> >,18)

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
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Int,Matrix<complex<float> >,Matrix<complex<float> >,3) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Int,Matrix<complex<double> >,Matrix<complex<double> >,4) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Float,Matrix<int>,Matrix<float>,5) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Float,Matrix<float>,Matrix<float>,6) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Float,Matrix<double>,Matrix<double>,7) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Float,Matrix<complex<float> >,Matrix<complex<float> >,8) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Float,Matrix<complex<double> >,Matrix<complex<double> >,9) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Double,Matrix<int>,Matrix<double>,10) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Double,Matrix<float>,Matrix<double>,11) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Double,Matrix<double>,Matrix<double>,12) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Double,Matrix<complex<float> >,Matrix<complex<double> >,13) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Double,Matrix<complex<double> >,Matrix<complex<double> >,14) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Complex<float>,Matrix<int>,Matrix<complex<float> >,15) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Complex<float>,Matrix<float>,Matrix<complex<float> >,16) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Complex<float>,Matrix<double>,Matrix<complex<double> >,17) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Complex<float>,Matrix<complex<float> >,Matrix<complex<float> >,18) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Complex<float>,Matrix<complex<double> >,Matrix<complex<double> >,19) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Complex<double>,Matrix<int>,Matrix<complex<double> >,20) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Complex<double>,Matrix<float>,Matrix<complex<double> >,21) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Complex<double>,Matrix<double>,Matrix<complex<double> >,22) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Complex<double>,Matrix<complex<float> >,Matrix<complex<double> >,23) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Complex<double>,Matrix<complex<double> >,Matrix<complex<double> >,24) 


#define REGISTER_ALL_MATRIX_SCALAR_VTABLE(table,function) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Matrix<int>,Int,Matrix<int>,0) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Matrix<float>,Int,Matrix<float>,1) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Matrix<double>,Int,Matrix<double>,2) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Matrix<complex<float> >,Int,Matrix<complex<float> >,3) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Matrix<complex<double> >,Int,Matrix<complex<double> >,4) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Matrix<int>,Float,Matrix<float>,5) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Matrix<float>,Float,Matrix<float>,6) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Matrix<double>,Float,Matrix<double>,7) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Matrix<complex<float> >,Float,Matrix<complex<float> >,8) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Matrix<complex<double> >,Float,Matrix<complex<double> >,9) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Matrix<int>,Double,Matrix<double>,10) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Matrix<float>,Double,Matrix<double>,11) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Matrix<double>,Double,Matrix<double>,12) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Matrix<complex<float> >,Double,Matrix<complex<double> >,13) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Matrix<complex<double> >,Double,Matrix<complex<double> >,14) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Matrix<int>,Complex<float>,Matrix<complex<float> >,15) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Matrix<float>,Complex<float>,Matrix<complex<float> >,16) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Matrix<double>,Complex<float>,Matrix<complex<double> >,17) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Matrix<complex<float> >,Complex<float>,Matrix<complex<float> >,18) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Matrix<complex<double> >,Complex<float>,Matrix<complex<double> >,19) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Matrix<int>,Complex<double>,Matrix<complex<double> >,20) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Matrix<float>,Complex<double>,Matrix<complex<double> >,21) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Matrix<double>,Complex<double>,Matrix<complex<double> >,22) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Matrix<complex<float> >,Complex<double>,Matrix<complex<double> >,23) \
REGISTER_DOUBLE_VTABLE_TEMPLATE(table,function,Matrix<complex<double> >,Complex<double>,Matrix<complex<double> >,24) 

