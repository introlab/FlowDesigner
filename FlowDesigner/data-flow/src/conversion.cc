// Copyright (C) 2003 Dominic Letourneau (dominic.letourneau@usherbrooke.ca)
#include "ObjectRef.h"
#include "Vector.h"
#include "operators.h"
#include "net_types.h"
#include "vmethod.h"
#include <sstream>
#include "typemap.h"
#include "conversion.h"
#include "Complex.h"
#include <sstream>
#include <iostream>
#include <complex>

using namespace std;

//@implements core


TypeMap<TypeMap<conv_func> > &Conversion::conv_table()
{
   static TypeMap<TypeMap<conv_func> > table;
   return table;
}

/*This doesn't compile with MSVC++ broken templates*/
#ifndef BROKEN_TEMPLATES

ObjectRef ReturnNilObject(ObjectRef in) 
{
  return nilObject;
}

template<class T, class U>
ObjectRef CTypeConversion(ObjectRef in)
{
  RCPtr<T> ObjectValue = in;
  return ObjectRef(U::alloc(static_cast<typename U::basicType>(ObjectValue->val())));
}

template <class T, class U>
ObjectRef CTypeStringConversion(ObjectRef in) 
{
  ostringstream out;
  in->prettyPrint(out);
  return ObjectRef(new U(out.str()));
}

template <class T, class U>
ObjectRef StringCTypeConversion(ObjectRef in) 
{
  RCPtr<T> ObjectValue = in;
  istringstream my_stream;
  my_stream.str(*ObjectValue);
  typename U::basicType value;
  my_stream >> value;
  return ObjectRef(new U(value));
}


template <class T, class U>
ObjectRef CTypeVectorConversion (ObjectRef in) {
  RCPtr<T> FromCType = in;
  RCPtr<U> ToVector(U::alloc(1));
  (*ToVector)[0] = static_cast<typename U::basicType>(FromCType->val());
  return ToVector;
}


//(DL) 17/02/2004
//Commented conversions make no sense to be implemented (?)

//to Bool conversion
REGISTER_CONVERSION_TEMPLATE(Bool, Bool, CTypeConversion);
REGISTER_CONVERSION_TEMPLATE(Int, Bool, CTypeConversion);
REGISTER_CONVERSION_TEMPLATE(Float, Bool, CTypeConversion);
REGISTER_CONVERSION_TEMPLATE(Double, Bool, CTypeConversion);
REGISTER_CONVERSION_TEMPLATE(String, Bool, StringCTypeConversion);
//REGISTER_CONVERSION_TEMPLATE(Complex<float>, Bool, CTypeConversion);
//REGISTER_CONVERSION_TEMPLATE(Complex<double>, Bool, CTypeConversion);
REGISTER_CONVERSION(NilObject, Bool, ReturnNilObject);

//to Int conversion
REGISTER_CONVERSION_TEMPLATE(Bool, Int, CTypeConversion);
REGISTER_CONVERSION_TEMPLATE(Int, Int, CTypeConversion);
REGISTER_CONVERSION_TEMPLATE(Float, Int, CTypeConversion);
REGISTER_CONVERSION_TEMPLATE(Double, Int, CTypeConversion);
REGISTER_CONVERSION_TEMPLATE(String, Int, StringCTypeConversion);
//REGISTER_CONVERSION_TEMPLATE(Complex<float>, Int, CTypeConversion);
//REGISTER_CONVERSION_TEMPLATE(Complex<double>, Int, CTypeConversion);
REGISTER_CONVERSION(NilObject, Int, ReturnNilObject);

//to Float conversion
REGISTER_CONVERSION_TEMPLATE(Bool, Float, CTypeConversion);
REGISTER_CONVERSION_TEMPLATE(Int, Float, CTypeConversion);
REGISTER_CONVERSION_TEMPLATE(Float, Float, CTypeConversion);
REGISTER_CONVERSION_TEMPLATE(Double, Float, CTypeConversion);
REGISTER_CONVERSION_TEMPLATE(String, Float, StringCTypeConversion);
//REGISTER_CONVERSION_TEMPLATE(Complex<float>, Float, CTypeConversion);
//REGISTER_CONVERSION_TEMPLATE(Complex<double>, Float, CTypeConversion);
REGISTER_CONVERSION(NilObject, Float, ReturnNilObject);

//to Double conversion
REGISTER_CONVERSION_TEMPLATE(Bool, Double, CTypeConversion);
REGISTER_CONVERSION_TEMPLATE(Int, Double, CTypeConversion);
REGISTER_CONVERSION_TEMPLATE(Float, Double, CTypeConversion);
REGISTER_CONVERSION_TEMPLATE(Double, Double, CTypeConversion);
REGISTER_CONVERSION_TEMPLATE(String, Double, StringCTypeConversion);
//REGISTER_CONVERSION_TEMPLATE(Complex<float>, Double, CTypeConversion);
//REGISTER_CONVERSION_TEMPLATE(Complex<double>, Double, CTypeConversion);
REGISTER_CONVERSION(NilObject, Double, ReturnNilObject);

//to Complex<float> conversion
REGISTER_CONVERSION_TEMPLATE(Bool, Complex<float>, CTypeConversion);
REGISTER_CONVERSION_TEMPLATE(Int, Complex<float>, CTypeConversion);
REGISTER_CONVERSION_TEMPLATE(Float, Complex<float>, CTypeConversion);
REGISTER_CONVERSION_TEMPLATE(Double, Complex<float>, CTypeConversion);
REGISTER_CONVERSION_TEMPLATE(String, Complex<float>, StringCTypeConversion);
REGISTER_CONVERSION_TEMPLATE(Complex<float>, Complex<float>, CTypeConversion);
//REGISTER_CONVERSION_TEMPLATE(Complex<double>, Complex<float>, CTypeConversion);
REGISTER_CONVERSION(NilObject, Complex<float>, ReturnNilObject);

//to Complex<double> conversion
REGISTER_CONVERSION_TEMPLATE(Bool, Complex<double>, CTypeConversion);
REGISTER_CONVERSION_TEMPLATE(Int, Complex<double>, CTypeConversion);
REGISTER_CONVERSION_TEMPLATE(Float, Complex<double>, CTypeConversion);
REGISTER_CONVERSION_TEMPLATE(Double, Complex<double>, CTypeConversion);
REGISTER_CONVERSION_TEMPLATE(String, Complex<double>, StringCTypeConversion);
REGISTER_CONVERSION_TEMPLATE(Complex<float>, Complex<double>, CTypeConversion);
REGISTER_CONVERSION_TEMPLATE(Complex<double>, Complex<double>, CTypeConversion);
REGISTER_CONVERSION(NilObject, Complex<double>, ReturnNilObject);

//to String conversion
REGISTER_CONVERSION_TEMPLATE(Bool, String, CTypeStringConversion);
REGISTER_CONVERSION_TEMPLATE(Int, String, CTypeStringConversion);
REGISTER_CONVERSION_TEMPLATE(Float, String, CTypeStringConversion);
REGISTER_CONVERSION_TEMPLATE(Double, String, CTypeStringConversion);
REGISTER_CONVERSION_TEMPLATE(Complex<float>, String, CTypeStringConversion);
REGISTER_CONVERSION_TEMPLATE(Complex<double>, String, CTypeStringConversion);
REGISTER_CONVERSION_TEMPLATE(String, String, CTypeStringConversion);
REGISTER_CONVERSION(NilObject, String, ReturnNilObject);

//to Vector conversion
//REGISTER_CONVERSION_TEMPLATE(Bool, Vector<bool>, CTypeStringConversion);
REGISTER_CONVERSION_TEMPLATE(Int, Vector<int>, CTypeVectorConversion);
REGISTER_CONVERSION_TEMPLATE(Float, Vector<float>, CTypeVectorConversion);
REGISTER_CONVERSION_TEMPLATE(Double, Vector<double>, CTypeVectorConversion);
REGISTER_CONVERSION_TEMPLATE(Complex<float>, Vector<complex<float> >, CTypeVectorConversion);
REGISTER_CONVERSION_TEMPLATE(Complex<double>, Vector<complex<double> >, CTypeVectorConversion);

//NilObject to Vector<T> returns nilObject...
REGISTER_CONVERSION(NilObject, Vector<int>, ReturnNilObject);
REGISTER_CONVERSION(NilObject, Vector<float>, ReturnNilObject);
REGISTER_CONVERSION(NilObject, Vector<double>, ReturnNilObject);
REGISTER_CONVERSION(NilObject, Vector<complex<float> >, ReturnNilObject);
REGISTER_CONVERSION(NilObject, Vector<complex<double> >, ReturnNilObject);


//(DL) 17/02/2004
//OLD conversion (to be removed someday)

//Int conversion
template<class T>
ObjectRef IntCTypeConversion(ObjectRef in)
{
   typedef typename T::basicType BaseType;
   BaseType f=dereference_cast<BaseType> (in);
   return ObjectRef(Int::alloc((int)f));
}

ObjectRef IntStringConversion(ObjectRef in) 
{
  String my_string = object_cast<String>(in);
  return ObjectRef(Int::alloc(atoi(my_string.c_str())));
}

REGISTER_VTABLE0(toInt, Int, IntCTypeConversion<Int>, 1);
REGISTER_VTABLE0(toInt, Bool, IntCTypeConversion<Bool>, 2);
REGISTER_VTABLE0(toInt, Float, IntCTypeConversion<Float>, 3);
REGISTER_VTABLE0(toInt, Double, IntCTypeConversion<Double>, 4);
REGISTER_VTABLE0(toInt, String, IntStringConversion, 5);
REGISTER_VTABLE0(toInt, NilObject, ReturnNilObject, 6);

//Bool conversion
template<class T>
ObjectRef BoolCTypeConversion(ObjectRef in)
{
   typedef typename T::basicType BaseType;
   BaseType f=dereference_cast<BaseType> (in);
   return ObjectRef(Bool::alloc((bool)f));
}

ObjectRef BoolStringConversion(ObjectRef in) 
{
  String my_string = object_cast<String>(in);
  return ObjectRef(Bool::alloc((bool)atoi(my_string.c_str())));
}

REGISTER_VTABLE0(toBool, Int, BoolCTypeConversion<Int>, 1);
REGISTER_VTABLE0(toBool, Bool, BoolCTypeConversion<Bool>, 2);
REGISTER_VTABLE0(toBool, Float, BoolCTypeConversion<Float>, 3);
REGISTER_VTABLE0(toBool, Double, BoolCTypeConversion<Double>, 4);
REGISTER_VTABLE0(toBool, String, BoolStringConversion, 5);
REGISTER_VTABLE0(toBool, NilObject, ReturnNilObject, 6);

//Float conversion
template<class T>
ObjectRef FloatCTypeConversion(ObjectRef in)
{
   typedef typename T::basicType BaseType;
   BaseType f=dereference_cast<BaseType> (in);
   return ObjectRef(Float::alloc((float)f));
}

ObjectRef FloatStringConversion(ObjectRef in) 
{
  String my_string = object_cast<String>(in);
  return ObjectRef(Float::alloc((float)atof(my_string.c_str())));
}

REGISTER_VTABLE0(toFloat, Int, FloatCTypeConversion<Int>, 1);
REGISTER_VTABLE0(toFloat, Bool, FloatCTypeConversion<Bool>, 2);
REGISTER_VTABLE0(toFloat, Float, FloatCTypeConversion<Float>, 3);
REGISTER_VTABLE0(toFloat, Double, FloatCTypeConversion<Double>, 4);
REGISTER_VTABLE0(toFloat, String, FloatStringConversion, 5);
REGISTER_VTABLE0(toFloat, NilObject, ReturnNilObject, 6);

//Double conversion
template<class T>
ObjectRef DoubleCTypeConversion(ObjectRef in)
{
   typedef typename T::basicType BaseType;
   BaseType f=dereference_cast<BaseType> (in);
   return ObjectRef(Double::alloc((double)f));
}

ObjectRef DoubleStringConversion(ObjectRef in) 
{
  String my_string = object_cast<String>(in);
  return ObjectRef(Double::alloc((double)atof(my_string.c_str())));
}

REGISTER_VTABLE0(toDouble, Int, DoubleCTypeConversion<Int>, 1);
REGISTER_VTABLE0(toDouble, Bool, DoubleCTypeConversion<Bool>, 2);
REGISTER_VTABLE0(toDouble, Float, DoubleCTypeConversion<Float>, 3);
REGISTER_VTABLE0(toDouble, Double, DoubleCTypeConversion<Double>, 4);
REGISTER_VTABLE0(toDouble, String, DoubleStringConversion, 5);
REGISTER_VTABLE0(toDouble, NilObject, ReturnNilObject, 6);


//String conversion
template<class T>
ObjectRef StringCTypeConversion(ObjectRef in)
{
   typedef typename T::basicType BaseType;
   BaseType f=dereference_cast<BaseType> (in);
   ostringstream my_stream;
   my_stream<<f;
   return ObjectRef(new String(my_stream.str()));
}

ObjectRef StringStringConversion(ObjectRef in) 
{
  String my_string = object_cast<String>(in);
  return ObjectRef(new String(my_string));
}

REGISTER_VTABLE0(toString, Int, StringCTypeConversion<Int>, 1);
REGISTER_VTABLE0(toString, Bool, StringCTypeConversion<Bool>, 2);
REGISTER_VTABLE0(toString, Float, StringCTypeConversion<Float>, 3);
REGISTER_VTABLE0(toString, Double, StringCTypeConversion<Double>, 4);
REGISTER_VTABLE0(toString, String, StringStringConversion, 5);
REGISTER_VTABLE0(toString, NilObject, ReturnNilObject, 6);


//Conversion to vector from a basic C type
template<class T>
ObjectRef VectorCTypeConversion(ObjectRef in)
{
 

   typedef typename T::basicType BaseType;
   BaseType value=dereference_cast<BaseType> (in);

   T &object = object_cast<T>(in);

   //create vector
   Vector<BaseType> *vect = new Vector<BaseType>(1,(bool) value);

   //return vector
   return ObjectRef(vect);
}

//Conversion to vector from a string
ObjectRef VectorStringConversion(ObjectRef in) {

  String &value = object_cast<String>(in);

  //create vector
  Vector<String> *vect = new Vector<String>(1,value);
  
  //return vector
  return ObjectRef(vect);
}

REGISTER_VTABLE0(toVect, Int, VectorCTypeConversion<Int>, 1);
//REGISTER_VTABLE0(toVect, Bool, VectorCTypeConversion<Bool>, 2);
REGISTER_VTABLE0(toVect, Float, VectorCTypeConversion<Float>, 3);
REGISTER_VTABLE0(toVect, Double, VectorCTypeConversion<Double>, 4);
REGISTER_VTABLE0(toVect, String, VectorStringConversion, 5);
REGISTER_VTABLE0(toVect, NilObject, ReturnNilObject, 6);

#endif //BROKEN_TEMPLATES
