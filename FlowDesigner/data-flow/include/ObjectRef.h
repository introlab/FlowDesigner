// Copyright (C) 1999 Jean-Marc Valin & Dominic Letourneau

#ifndef OBJECTREF_H
#define OBJECTREF_H


#include "Object.h"
#include "net_types.h"
#include "BaseException.h"
#include <typeinfo>

namespace FD {

/** 
    The CastException occurs when we are unable to cast an ObjectRef.
    @author Jean-Marc Valin.
    @version 1.0
 */
template <class T>
class CastException : public GenericCastException {
public:
   /**The constructur that takes an error message*/
   CastException(std::string _type) : type(_type) 
   {} 
  
   /**The print method*/
   virtual void print(std::ostream &out = std::cerr) 
   {
      out << "Cast error: Trying to cast ObjectRef (" << type << ") into " << typeid(T).name() << std::endl;
   }

protected:
   /**The error message*/
   std::string type;
};

/**The object pointer cast from ObjectRef*/
template <class T>
inline T object_ptr_cast (const ObjectRef &ref)
{
   T tmp = dynamic_cast<T>(&(*ref));
   if (!tmp) 
      throw new CastException<T> (typeid ((ref.get())).name());
   return tmp;
}

/**This is the default (and slowest) implementation*/
template <class T>
inline T &object_cast (const ObjectRef &ref)
{
   T *tmp = dynamic_cast<T *>(&(*ref));
   if (!tmp) 
      throw new CastException<T> (typeid ((ref.get())).name());
   return *tmp;
}

/**The type cast from ObjectRef*/
template <class T>
inline T &dereference_cast (const ObjectRef &ref)
{
   GenericType<T> *tmp = (dynamic_cast<GenericType<T> * >(&(*ref)));
   if (!tmp) 
      throw new CastException<T> (typeid ((ref.get())).name());
   return tmp->val();
}


/**Does the object derive from T*/
template <class T>
inline bool &object_kind_of (const ObjectRef &ref)
{
   return dynamic_cast<T *>(&(*ref));
}


/**Is the object of type T (exact match)*/
template <class T>
inline bool object_has_type (const ObjectRef &ref)
{
   return typeid(ref.get()) == typeid(T);
}

}//end namespace FD

#endif
