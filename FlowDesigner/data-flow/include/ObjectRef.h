#ifndef OBJECTREF_H
#define OBJECTREF_H


#include "Object.h"
#include "net_types.h"
#include "Exception.h"
#include <typeinfo>

/** Smart pointer to Object called ObjectRef
    @author Jean-Marc Valin
    @version 1.0
 */
typedef counted_ptr<Object> ObjectRef; 

/** 
    The CastException occurs when we are unable to cast an ObjectRef.
    @author Jean-Marc Valin.
    @version 1.0
 */
template <class T>
class CastException : public BaseException {
public:
   ///The constructur that takes an error message
   CastException(string _type) : type(_type) 
   {}   
   ///The print method.
   virtual void print(ostream &out = cerr) 
   {
      out << "Cast error: Trying to cast ObjectRef (" << type << ") into " << typeid(T).name() << endl;
   }

protected:
   ///The error message
   string type;
};

///The object pointer cast from ObjectRef
template <class T>
T object_ptr_cast (const ObjectRef &ref)
{
   T tmp = dynamic_cast<T>(&(*ref));
   if (!tmp) 
      throw new CastException<T> (typeid ((*ref)).name());
   return tmp;
}

///The object cast from ObjectRef
template <class T>
T &object_cast (const ObjectRef &ref)
{
   T *tmp = dynamic_cast<T *>(&(*ref));
   if (!tmp) 
      throw new CastException<T> (typeid ((*ref)).name());
   return *tmp;
}

///The type cast from ObjectRef
template <class T>
T &dereference_cast (const ObjectRef &ref)
{
   GenericType<T> *tmp = (dynamic_cast<GenericType<T> * >(&(*ref)));
   if (!tmp) 
      throw new CastException<T> (typeid ((*ref)).name());
   return tmp->val();
}








#endif
