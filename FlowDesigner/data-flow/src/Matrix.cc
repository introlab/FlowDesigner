// Copyright (C) 2001 Jean-Marc Valin

#include "Matrix.h"
#include "Object.h"
#include "ObjectParser.h"

//@implements core

//static int dummy = Object::addObjectType<Matrix<float> > ("Matrix", new ObjectFactory<Matrix<float> > ("Matrix"));
DECLARE_TYPE3("Matrix", Matrix<float>, 0)
DECLARE_TYPE2(Matrix<float>, 1)
