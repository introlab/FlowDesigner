// Copyright (C) 2001 Jean-Marc Valin

#include "Matrix.h"
#include "Object.h"
#include "ObjectParser.h"

static int dummy = Object::addObjectType<Matrix<float> > ("Matrix", new ObjectFactory<Matrix<float> > ("Matrix"));
