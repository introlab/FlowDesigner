// Copyright (C) 1999 Jean-Marc Valin & Dominic Letourneau

#ifndef OBJECTREF_H
#define OBJECTREF_H


#include "Object.h"
#include "net_types.h"
#include "BaseException.h"
#include <typeinfo>



/** 
    The CastException occurs when we are unable to cast an ObjectRef.
    @author Jean-Marc Valin.
    @version 1.0
 */
template <class T>
class CastException : public GenericCastException {
public:
   /**The constructur that takes an error message*/
   CastException(string _type) : type(_type) 
   {} 
  
   /**The print method*/
   virtual void print(ostream &out = cerr) 
   {
      out << "Cast error: Trying to cast ObjectRef (" << type << ") into " << typeid(T).name() << endl;
   }

protected:
   /**The error message*/
   string type;
};

/**The object pointer cast from ObjectRef*/
template <class T>
inline T object_ptr_cast (const ObjectRef &ref)
{
   T tmp = dynamic_cast<T>(&(*ref));
   if (!tmp) 
      throw new CastException<T> (typeid ((*ref)).name());
   return tmp;
}

/**Different implementations for object_cast<T>, a cast from ObjectRef to any kind of derived class*/
#if defined (FORCE_OBJECT_CAST)

/**This is the fastest and most dangerous method. It WILL CRASH is the cast fails*/
template <class T>
inline T &object_cast(const ObjectRef &ref)
{
   return *(T*) (int(&(*ref))-int(static_cast<Object*>((T*)(1)))+1);
}

#elif defined (FAST_OBJECT_CAST)

/*This is a faster (that dynamic_cast) implementation, but I'm not sure it's 100% portable*/
template <class T>
inline T &object_cast(const ObjectRef &ref)
{
   //Do we have an exact match? If so, proceed with fast method
   if (typeid(T) == typeid(*ref))
   {
      //This is a reinterpret cast with a kludged offset adjustment.
      return *(T*) (int(&(*ref))-int(static_cast<Object*>((T*)(1)))+1);
   } else 
   {
      T *tmp = dynamic_cast<T *>(&(*ref));
      if (!tmp) 
	 throw new CastException<T> (typeid ((*ref)).name());
      return *tmp;
   }
}

#else

/**This is the default (and slowest) implementation*/
template <class T>
inline T &object_cast (const ObjectRef &ref)
{
   T *tmp = dynamic_cast<T *>(&(*ref));
   if (!tmp) 
      throw new CastException<T> (typeid ((*ref)).name());
   return *tmp;
}
#endif



/**The type cast from ObjectRef*/
template <class T>
inline T &dereference_cast (const ObjectRef &ref)
{
   GenericType<T> *tmp = (dynamic_cast<GenericType<T> * >(&(*ref)));
   if (!tmp) 
      throw new CastException<T> (typeid ((*ref)).name());
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
inline bool &object_has_type (const ObjectRef &ref)
{
   return typeid(*ref) == typeid(T);
}



#endif
