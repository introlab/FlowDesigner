#include "Matrix.h"
#include "Object.h"
#include "ObjectParser.h"

static int dummy = Object::addObjectType ("Matrix", new ObjectFactory<Vector<float> >);
