// Copyright (C) 2001 Jean-Marc Valin

#include "Vector.h"
#include "Object.h"
#include "ObjectParser.h"
#include "VectorPool.h"

#include "ObjectRef.h"
#include "DoubleDispatch.h"
#include "operators.h"
#include "vec.h"
#include <complex>
#include "Complex.h"

//@implements core

using namespace std;

namespace FD {

//static int dummy = Object::addObjectType<Vector<float> > ("Vector", new ObjectFactory<Vector<float> > ("Vector"));
DECLARE_TYPE2("Vector", Vector<float>)
DECLARE_TYPE(Vector<float>)
DECLARE_TYPE(Vector<double>)
DECLARE_TYPE(Vector<int>)
//DECLARE_TYPE(Vector<bool>)
DECLARE_TYPE(Vector<ObjectRef>)
DECLARE_TYPE2("Vector<complex<float>>", Vector<complex<float> >)
DECLARE_TYPE2("Vector<complex<double>>", Vector<complex<double> >)
DECLARE_TYPE(Vector<string>)
DECLARE_TYPE(Vector<String>)

VectorPool<float> floatVectorPool;
VectorPool<double> doubleVectorPool;

#ifndef __CYGWIN__

//pretty print specialization
template<>
void Vector<float>::prettyPrint(std::ostream &out) const {
  for (unsigned int i = 0; i < size(); i++) {
    out<<(*this)[i]<<" ";
  }
  out<<std::endl;
}

template<>
void Vector<double>::prettyPrint(std::ostream &out) const {
  for (unsigned int i = 0; i < size(); i++) {
    out<<(*this)[i]<<" ";
  }
  out<<std::endl;
}

template<>
void Vector<int>::prettyPrint(std::ostream &out) const {
  for (unsigned int i = 0; i < size(); i++) {
    out<<(*this)[i]<<" ";
  }
  out<<std::endl;
}

#if 0
void Vector<bool>::prettyPrint(std::ostream &out) const {
  for (unsigned int i = 0; i < size(); i++) {
    out<<(*this)[i]<<" ";
  }
  out<<std::endl;
}
#endif

template<>
void Vector<ObjectRef>::prettyPrint(std::ostream &out) const {
  for (unsigned int i = 0; i < size(); i++) {
    (*this)[i]->prettyPrint(out);
    out<<std::endl;
  } 
}

template<>
void Vector<std::complex<float> >::prettyPrint(std::ostream &out) const {
  for (unsigned int i = 0; i < size(); i++) {
    out<<(*this)[i]<<" ";
  }
  out<<std::endl;
}
template<>
void Vector<std::complex<double> >::prettyPrint(std::ostream &out) const {
  for (unsigned int i = 0; i < size(); i++) {
    out<<(*this)[i]<<" ";
  }
  out<<std::endl;
}

template<>
void Vector<std::string>::prettyPrint(std::ostream &out) const {
  for (unsigned int i = 0; i < size(); i++) {
    out<<(*this)[i]<<std::endl;
  }
}

template<>
void Vector<String>::prettyPrint(std::ostream &out) const {
  for (unsigned int i = 0; i < size(); i++) {
    out<<(*this)[i]<<std::endl;
  }
}



#endif



}//namespace FD
