// Copyright (C) 1999 Jean-Marc Valin


#ifndef TYPE_TRAITS_H
#define TYPE_TRAITS_H

#include "Object.h"


/* KLUDGE: This is a workaround for (I think) a bug in gcc 2.96 where you cannot 
   include <complex> if you add -I/usr/include to the include path... strange...*/
#ifdef __GNUC__
#if (__GNUC__ == 2 && __GNUC_MINOR__ == 96)
#define GCC_296_COMPLEX_KLUDGE
#endif
#endif

#ifdef GCC_296_COMPLEX_KLUDGE
template<class T>
class complex;
#else
#include <complex>
#endif

class TTraits {
public:
   enum Kind {Object=0, Basic=1, Unknown=2, ObjectPointer=3, BasicPointer=4, UnknownPointer=5};
};

template<class T>
struct TypeTraits {
      enum {kind=TTraits::Unknown};
};

#define _DEF_OBJECT_TYPE(type) template<>struct TypeTraits<type> {enum {kind=TTraits::Object};};
#define _DEF_OBJECTPTR_TYPE(type) template<>struct TypeTraits<type> {enum {kind=TTraits::ObjectPointer};};
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

_DEF_OBJECTPTR_TYPE(ObjectRef)

//_DEF_C_TYPE(string)

#ifndef BROKEN_TEMPLATES

template<class T>struct TypeTraits<complex<T> > {enum {kind=TTraits::Basic};};
template<class T>struct TypeTraits<RCPtr<T> > {enum {kind=TTraits::ObjectPointer};};

//KLUDGE: This is a kludge but it should get the right traits for pointer types
template<class T>struct TypeTraits<T*> {enum {kind=TTraits::ObjectPointer+TypeTraits<T>::kind};};

#endif /*ifndef BROKEN_TEMPLATES*/


#endif
