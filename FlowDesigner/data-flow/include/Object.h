#ifndef _OBJECT_H_
#define _OBJECT_H_

#include "rc_ptrs.h"
#include <string>

//#define object_cast(_type,_objref) (*(dynamic_cast<_type *>(&(*_objref))))

class Object {

public:
   enum ObjectStatus {valid=0, before_beginning, erased, past_end, compute_error, rt_part_end, nil};
   Object() :status(valid){} //constructor
   Object(ObjectStatus st) :status(st){} //constructor
   virtual ~Object() { } //destructor
   ObjectStatus status;
protected:
   
};

#include "net_types.h"

typedef counted_ptr<Object> ObjectRef; //Smart pointer to Object

template <class T>
T object_ptr_cast (const ObjectRef &ref)
{
   T tmp = dynamic_cast<T>(&(*ref));
   if (!tmp) throw (string("T* dynamic_cast error in ObjectRef"));
   return tmp;
}

template <class T>
T &object_cast (const ObjectRef &ref)
{
   T *tmp = dynamic_cast<T *>(&(*ref));
   if (!tmp) throw (string("T& dynamic_cast error in ObjectRef"));
   return *tmp;
}

template <class T>
T &dereference_cast (const ObjectRef &ref)
{
   T *tmp = reinterpret_cast<T *> (dynamic_cast<GenericType<T> * >(&(*ref)));
   if (!tmp) throw (string("T& dynamic_cast error in ObjectRef"));
   return *tmp;
}



#endif
