// Copyright (C) 1999 Jean-Marc Valin


#ifndef TYPE_TRAITS_H
#define TYPE_TRAITS_H

#include "Object.h"
//#include <complex>

template<class T>
class complex;
//class string;

class TTraits {
public:
   enum Kind {Object=0, Basic=1, Unknown=2, Pointer=3};
};

template<class T>
struct TypeTraits {
      enum {kind=TTraits::Unknown};
};

#define _DEF_OBJECT_TYPE(type) template<>struct TypeTraits<type> {enum {kind=TTraits::Object};};
#define _DEF_C_TYPE(type) template<>struct TypeTraits<type> {enum {kind=TTraits::Basic};};

//#define _DEF_UNKNOWN_TYPE(type) template<>struct TypeTraits<type> {enum {isBasic=2};};

_DEF_C_TYPE(int)
_DEF_C_TYPE(float)
_DEF_C_TYPE(double)
_DEF_C_TYPE(bool)
_DEF_C_TYPE(unsigned int)
_DEF_C_TYPE(char)
_DEF_C_TYPE(unsigned char)
_DEF_C_TYPE(long)
_DEF_C_TYPE(unsigned long)
_DEF_C_TYPE(complex<float>)
_DEF_C_TYPE(complex<double>)
_DEF_C_TYPE(string)
//_DEF_UNKNOWN_TYPE(ObjectRef)

//template<class T>struct TypeTraits<T*> {enum {isBasic=2};};
//template<class T>struct TypeTraits<RCPtr<T> > {enum {isBasic=2};};
template<class T>struct TypeTraits<complex<T> > {enum {kind=TTraits::Basic};};


#endif
