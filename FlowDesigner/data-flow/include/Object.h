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


#endif
