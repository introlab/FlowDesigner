// Copyright (C) 2001 Jean-Marc Valin
// Copyright (C) 2004 Dominic Letourneau

#include "Matrix.h"
#include "Object.h"
#include "ObjectParser.h"
#include <complex>
#include "operators.h"
#include "Complex.h"

//@implements core

DECLARE_TYPE2("Matrix", Matrix<float>)
DECLARE_TYPE(Matrix<bool>)
DECLARE_TYPE(Matrix<int>)
DECLARE_TYPE(Matrix<float>)
DECLARE_TYPE(Matrix<double>)
DECLARE_TYPE2("Matrix<complex<float>>",Matrix<complex<float> >)
DECLARE_TYPE2("Matrix<complex<double>>",Matrix<complex<double> >)
DECLARE_TYPE(Matrix<ObjectRef>)
DECLARE_TYPE(Matrix<string>)
DECLARE_TYPE(Matrix<String>)


