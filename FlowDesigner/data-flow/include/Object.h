#ifndef _OBJECT_H_
#define _OBJECT_H_

#include "rc_ptrs.h"
#include <string>

//#define object_cast(_type,_objref) (*(dynamic_cast<_type *>(&(*_objref))))
/** Our Object base class. Everything in the network must have Object as 
    base class.
    @author Dominic Letourneau & Jean-Marc Valin
*/
class Object {

public:

   ///The status of an object
   enum ObjectStatus {valid=0, before_beginning, erased, past_end, compute_error, rt_part_end, nil};
   ///default constructor
   Object() :status(valid){} //constructor
   ///constructor with a status
   Object(ObjectStatus st) :status(st){} //constructor
   ///destructor
   virtual ~Object() { } //destructor
   ///The current status
   ObjectStatus status;

protected:
   
};


#endif
