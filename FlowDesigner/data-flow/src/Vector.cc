#include "Vector.h"
#include "Object.h"
#include "ObjectParser.h"
#include "VectorPool.h"

#include "ObjectRef.h"
#include "DoubleDispatch.h"
#include "operators.h"

static int dummy = Object::addObjectType ("Vector", new ObjectFactory<Vector<float> >);

VectorPool<float> floatVectorPool;

VectorPool<double> doubleVectorPool;

ObjectRef addVectorFloat(ObjectRef x, ObjectRef y)
{
   Vector<float> &v1 = object_cast<Vector<float> > (x);
   Vector<float> &v2 = object_cast<Vector<float> > (y);
   if (v1.size() != v2.size())
      throw new GeneralException("Vector size mismatch", __FILE__, __LINE__);
   int length = v1.size();

   Vector<float> *v = Vector<float>::alloc(length);
   for (int i=0;i<length;i++)
      (*v)[i] = v1[i]+v2[i];
   return ObjectRef(v);
}
REGISTER_DOUBLE_VTABLE(addVtable, addVectorFloat, Vector<float>, Vector<float>);


ObjectRef mulVectorFloat(ObjectRef x, ObjectRef y)
{
   Vector<float> &v1 = object_cast<Vector<float> > (x);
   Vector<float> &v2 = object_cast<Vector<float> > (y);
   if (v1.size() != v2.size())
      throw new GeneralException("Vector size mismatch", __FILE__, __LINE__);
   int length = v1.size();

   Vector<float> *v = Vector<float>::alloc(length);
   for (int i=0;i<length;i++)
      (*v)[i] = v1[i]*v2[i];
   return ObjectRef(v);
}
REGISTER_DOUBLE_VTABLE(mulVtable, mulVectorFloat, Vector<float>, Vector<float>);


ObjectRef subVectorFloat(ObjectRef x, ObjectRef y)
{
   Vector<float> &v1 = object_cast<Vector<float> > (x);
   Vector<float> &v2 = object_cast<Vector<float> > (y);
   if (v1.size() != v2.size())
      throw new GeneralException("Vector size mismatch", __FILE__, __LINE__);
   int length = v1.size();

   Vector<float> *v = Vector<float>::alloc(length);
   for (int i=0;i<length;i++)
      (*v)[i] = v1[i]-v2[i];
   return ObjectRef(v);
}
REGISTER_DOUBLE_VTABLE(subVtable, subVectorFloat, Vector<float>, Vector<float>);


ObjectRef divVectorFloat(ObjectRef x, ObjectRef y)
{
   Vector<float> &v1 = object_cast<Vector<float> > (x);
   Vector<float> &v2 = object_cast<Vector<float> > (y);
   if (v1.size() != v2.size())
      throw new GeneralException("Vector size mismatch", __FILE__, __LINE__);
   int length = v1.size();

   Vector<float> *v = Vector<float>::alloc(length);
   for (int i=0;i<length;i++)
      (*v)[i] = v1[i]/v2[i];
   return ObjectRef(v);
}
REGISTER_DOUBLE_VTABLE(divVtable, divVectorFloat, Vector<float>, Vector<float>);
