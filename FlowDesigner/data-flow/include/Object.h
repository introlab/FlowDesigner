#ifndef _OBJECT_H_
#define _OBJECT_H_

#include "rc_ptrs.h"
#include <string>

/** Our Object base class. Everything in the network must have Object as 
    base class.
    @author Dominic Letourneau & Jean-Marc Valin
*/
class Object {

public:

   /**The status of an object*/
   enum ObjectStatus {valid=0, before_beginning, erased, past_end, compute_error, wait, nil};

   /**default constructor*/
   Object() :status(valid){}

   /**constructor with a status*/
   Object(ObjectStatus st) :status(st){}

   /**destructor*/
   virtual ~Object() { }

   /**The current status*/
   ObjectStatus status;

   /**Generic print function*/
   virtual void printOn(ostream &out=cout) const
   {
      if (typeid (*this) == typeid(Object))
         out << "<Object <status " << status << "> >" << endl;
      else 
         out << "<" << typeid (*this).name() << " <status " << status << "> >" << endl;
   }
   
   /**Prints the object to a stream*/
   friend ostream &operator << (ostream &out, const Object& obj) 
   {
      obj.printOn (out);
      return out;
   }
protected:
   
};


#endif
