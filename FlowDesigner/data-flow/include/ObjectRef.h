#ifndef OBJECTREF_H
#define OBJECTREF_H


#include "Object.h"
#include "net_types.h"
#include "NetworkException.h"
#include <typeinfo>
typedef counted_ptr<Object> ObjectRef; //Smart pointer to Object

template <class T>
class CastException : public NetworkBaseException {
public:
   CastException(string _type) : type(_type) 
   {}   

   virtual void print(ostream &out = cerr) 
   {
      out << "Cast error: Trying to cast ObjectRef (" << type << ") into " << typeid(T).name() << endl;
   }
protected:
   string type;
};


template <class T>
T object_ptr_cast (const ObjectRef &ref)
{
   T tmp = dynamic_cast<T>(&(*ref));
   if (!tmp) throw new CastException<T> (typeid ((*ref)).name());
   return tmp;
}

template <class T>
T &object_cast (const ObjectRef &ref)
{
   T *tmp = dynamic_cast<T *>(&(*ref));
   if (!tmp) throw new CastException<T> (typeid ((*ref)).name());
   return *tmp;
}

template <class T>
T &dereference_cast (const ObjectRef &ref)
{
   GenericType<T> *tmp = (dynamic_cast<GenericType<T> * >(&(*ref)));
   if (!tmp) throw new CastException<T> (typeid ((*ref)).name());
   return tmp->val();
}








#endif
