#include "Vector.h"
#include "Object.h"
#include "ObjectParser.h"
#include "VectorPool.h"

static int dummy = Object::addObjectType ("Vector", new ObjectFactory<Vector<float> >);

VectorPool<float> floatVectorPool;

VectorPool<double> doubleVectorPool;
