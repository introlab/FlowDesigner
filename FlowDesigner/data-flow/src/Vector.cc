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

#endif

}//namespace FD
