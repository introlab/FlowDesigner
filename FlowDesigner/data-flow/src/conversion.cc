// Copyright (C) 2003 Dominic Letourneau (dominic.letourneau@usherbrooke.ca)
#include "ObjectRef.h"
#include "Vector.h"
#include "operators.h"
#include "net_types.h"
#include "vmethod.h"
#include <sstream>

using namespace std;

//@implements core

/*This doesn't compile with MSVC++ broken templates*/
#ifndef BROKEN_TEMPLATES

ObjectRef ReturnNilObject(ObjectRef in) 
{
  return nilObject;
}

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
REGISTER_VTABLE0(toVect, Bool, VectorCTypeConversion<Bool>, 2);
REGISTER_VTABLE0(toVect, Float, VectorCTypeConversion<Float>, 3);
REGISTER_VTABLE0(toVect, Double, VectorCTypeConversion<Double>, 4);
REGISTER_VTABLE0(toVect, String, VectorStringConversion, 5);
REGISTER_VTABLE0(toVect, NilObject, ReturnNilObject, 6);

#endif
