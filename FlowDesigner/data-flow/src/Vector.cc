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

static int dummy = Object::addObjectType ("Vector", new ObjectFactory<Vector<float> >);

VectorPool<float> floatVectorPool;

VectorPool<double> doubleVectorPool;

ObjectRef addVectorFloat(ObjectRef x, ObjectRef y)
{
   Vector<float> &v1 = object_cast<Vector<float> > (x);
   Vector<float> &v2 = object_cast<Vector<float> > (y);
   if (v1.size() != v2.size())
   {
      cerr << v1.size() << " != " << v2.size() << endl;
      throw new GeneralException("Vector size mismatch", __FILE__, __LINE__);
   }
   int length = v1.size();

   Vector<float> *v = Vector<float>::alloc(length);
   vec_add_vec(&v1[0], &v2[0], &(*v)[0], length);
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
   vec_mul_vec(&v1[0], &v2[0], &(*v)[0], length);
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
   vec_sub_vec(&v1[0], &v2[0], &(*v)[0], length);
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
   vec_div_vec(&v1[0], &v2[0], &(*v)[0], length);
   return ObjectRef(v);
}
REGISTER_DOUBLE_VTABLE(divVtable, divVectorFloat, Vector<float>, Vector<float>);



ObjectRef addVectorFloatFloat (ObjectRef x, ObjectRef y)
{
   Vector<float> &v1 = object_cast<Vector<float> > (x);
   float v2 = dereference_cast<float> (y);
   int length = v1.size();

   Vector<float> *v = Vector<float>::alloc(length);
   vec_add_scal(v2, &v1[0], &(*v)[0], length);
   return ObjectRef(v);
}
REGISTER_DOUBLE_VTABLE(addVtable, addVectorFloatFloat, Vector<float>, Float);

ObjectRef subVectorFloatFloat (ObjectRef x, ObjectRef y)
{
   Vector<float> &v1 = object_cast<Vector<float> > (x);
   float v2 = dereference_cast<float> (y);
   int length = v1.size();

   Vector<float> *v = Vector<float>::alloc(length);
   vec_add_scal(-v2, &v1[0], &(*v)[0], length);
   return ObjectRef(v);
}
REGISTER_DOUBLE_VTABLE(subVtable, subVectorFloatFloat, Vector<float>, Float);

ObjectRef mulVectorFloatFloat (ObjectRef x, ObjectRef y)
{
   Vector<float> &v1 = object_cast<Vector<float> > (x);
   float v2 = dereference_cast<float> (y);
   int length = v1.size();

   Vector<float> *v = Vector<float>::alloc(length);
   vec_mul_scal(v2, &v1[0], &(*v)[0], length);
   return ObjectRef(v);
}
REGISTER_DOUBLE_VTABLE(mulVtable, mulVectorFloatFloat, Vector<float>, Float);



ObjectRef addVectorComplexFloat(ObjectRef x, ObjectRef y)
{
   Vector<complex<float> > &v1 = object_cast<Vector<complex<float> > > (x);
   Vector<complex<float> > &v2 = object_cast<Vector<complex<float> > > (y);
   if (v1.size() != v2.size())
   {
      cerr << v1.size() << " != " << v2.size() << endl;
      throw new GeneralException("Vector size mismatch", __FILE__, __LINE__);
   }
   int length = v1.size();

   Vector<complex<float> > *v = Vector<complex<float> >::alloc(length);
   vec_add_vec(&v1[0], &v2[0], &(*v)[0], length);
   return ObjectRef(v);
}
REGISTER_DOUBLE_VTABLE(addVtable, addVectorComplexFloat, Vector<complex<float> >, Vector<complex<float> >);


ObjectRef mulVectorComplexFloat(ObjectRef x, ObjectRef y)
{
   Vector<complex<float> > &v1 = object_cast<Vector<complex<float> > > (x);
   Vector<complex<float> > &v2 = object_cast<Vector<complex<float> > > (y);
   if (v1.size() != v2.size())
   {
      cerr << v1.size() << " != " << v2.size() << endl;
      throw new GeneralException("Vector size mismatch", __FILE__, __LINE__);
   }
   int length = v1.size();

   Vector<complex<float> > *v = Vector<complex<float> >::alloc(length);
   vec_mul_vec(&v1[0], &v2[0], &(*v)[0], length);
   return ObjectRef(v);
}
REGISTER_DOUBLE_VTABLE(mulVtable, mulVectorComplexFloat, Vector<complex<float> >, Vector<complex<float> >);


ObjectRef maxVectorFloat(ObjectRef x, ObjectRef y)
{
   Vector<float> &v1 = object_cast<Vector<float> > (x);
   Vector<float> &v2 = object_cast<Vector<float> > (y);
   if (v1.size() != v2.size())
   {
      cerr << v1.size() << " != " << v2.size() << endl;
      throw new GeneralException("Vector size mismatch", __FILE__, __LINE__);
   }
   int length = v1.size();

   Vector<float> *v = Vector<float>::alloc(length);
   for (int i=0;i<length;i++)
      (*v)[i] = max(v1[i], v2[i]);
   return ObjectRef(v);
}
REGISTER_DOUBLE_VTABLE(maxVtable, maxVectorFloat, Vector<float>, Vector<float>);


ObjectRef minVectorFloat(ObjectRef x, ObjectRef y)
{
   Vector<float> &v1 = object_cast<Vector<float> > (x);
   Vector<float> &v2 = object_cast<Vector<float> > (y);
   if (v1.size() != v2.size())
   {
      cerr << v1.size() << " != " << v2.size() << endl;
      throw new GeneralException("Vector size mismatch", __FILE__, __LINE__);
   }
   int length = v1.size();

   Vector<float> *v = Vector<float>::alloc(length);
   for (int i=0;i<length;i++)
      (*v)[i] = min(v1[i], v2[i]);
   return ObjectRef(v);
}
REGISTER_DOUBLE_VTABLE(minVtable, minVectorFloat, Vector<float>, Vector<float>);



ObjectRef concatVectorFloat(ObjectRef x, ObjectRef y)
{
   Vector<float> &v1 = object_cast<Vector<float> > (x);
   Vector<float> &v2 = object_cast<Vector<float> > (y);

   Vector<float> *v = Vector<float>::alloc(v1.size()+v2.size());
   vec_copy(&v1[0], &(*v)[0], v1.size());
   vec_copy(&v2[0], &(*v)[v1.size()], v2.size());
   return ObjectRef(v);
}
REGISTER_DOUBLE_VTABLE(concatVtable, concatVectorFloat, Vector<float>, Vector<float>);


ObjectRef concatVectorFloatFloat(ObjectRef x, ObjectRef y)
{
   Vector<float> &v1 = object_cast<Vector<float> > (x);
   float f = dereference_cast<float> (y);

   Vector<float> *v = Vector<float>::alloc(v1.size()+1);
   vec_copy(&v1[0], &(*v)[0], v1.size());
   (*v)[v1.size()] = f;
   return ObjectRef(v);
}
REGISTER_DOUBLE_VTABLE(concatVtable, concatVectorFloatFloat, Vector<float>, float);

ObjectRef concatFloatVectorFloat(ObjectRef x, ObjectRef y)
{
   Vector<float> &v1 = object_cast<Vector<float> > (y);
   float f = dereference_cast<float> (x);

   Vector<float> *v = Vector<float>::alloc(v1.size()+1);
   vec_copy(&v1[0], &(*v)[1], v1.size());
   (*v)[0] = f;
   return ObjectRef(v);
}
REGISTER_DOUBLE_VTABLE(concatVtable, concatFloatVectorFloat, float, Vector<float>);

ObjectRef concatFloatFloat(ObjectRef x, ObjectRef y)
{
   float f1 = dereference_cast<float> (x);
   float f2 = dereference_cast<float> (y);

   Vector<float> *v = Vector<float>::alloc(2);
   (*v)[0] = f1;
   (*v)[1] = f2;
   return ObjectRef(v);
}
REGISTER_DOUBLE_VTABLE(concatVtable, concatFloatFloat, float, float);
