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


