// Copyright (C) 1999 Jean-Marc Valin


#ifndef TYPE_TRAITS_H
#define TYPE_TRAITS_H

#include "Object.h"
#include <complex>

template<class T>
struct TypeTraits {
      enum {isBasic=0};
};

#define _DEF_C_TYPE(type) template<>struct TypeTraits<type> {enum {isBasic=1};};

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
_DEF_C_TYPE(ObjectRef)

/*
template<int I>
void impl() {cerr << "class\n";}

template<>
void impl<1>() {cerr << "c type\n";}


template<class T>
void something()
{
   impl<TypeTraits<T>::isBasic >();
}
*/

#endif
