#include "vmethod.h"
#include "net_types.h"
#include <math.h>
#include "ObjectRef.h"
#include "Vector.h"

template<class T>
ObjectRef logObj(ObjectRef in)
{
   typedef typename T::basicType BaseType;
   BaseType f=dereference_cast<BaseType> (in);
   return ObjectRef(new T(BaseType(log(f))));
}

template<class T>
ObjectRef expObj(ObjectRef in)
{
   typedef typename T::basicType BaseType;
   BaseType f=dereference_cast<BaseType> (in);
   return ObjectRef(new T(BaseType(exp(f))));
}

template<class T>
ObjectRef sinObj(ObjectRef in)
{
   typedef typename T::basicType BaseType;
   BaseType f=dereference_cast<BaseType> (in);
   return ObjectRef(new T(BaseType(sin(f))));
}

template<class T>
ObjectRef cosObj(ObjectRef in)
{
   typedef typename T::basicType BaseType;
   BaseType f=dereference_cast<BaseType> (in);
   return ObjectRef(new T(BaseType(cos(f))));
}

REGISTER_VTABLE0(log, Float, logObj<Float>, 1)
REGISTER_VTABLE0(log, Int, logObj<Int>, 2)

REGISTER_VTABLE0(exp, Float, expObj<Float>, 1)
REGISTER_VTABLE0(exp, Int, expObj<Int>, 2)

REGISTER_VTABLE0(sin, Float, sinObj<Float>, 1)
REGISTER_VTABLE0(sin, Int, sinObj<Int>, 2)

REGISTER_VTABLE0(cos, Float, cosObj<Float>, 1)
REGISTER_VTABLE0(cos, Int, cosObj<Int>, 2)

