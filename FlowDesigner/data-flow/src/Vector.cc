#include "Vector.h"
#include "Object.h"
#include "ObjectParser.h"

static int dummy = Object::addObjectType ("Vector", new ObjectFactory<Vector<float> >);

