// Copyright (C) 1999 Jean-Marc Valin

#ifndef _FLOW_EXCEPTION_H
#define _FLOW_EXCEPTION_H

#include "Object.h"
#include "ObjectRef.h"

namespace FD {

class FlowException : public Object {
  protected:
   ObjectRef thrown;
  public:
   FlowException(ObjectRef _thrown)
      : thrown(_thrown)
      {}
   ObjectRef getObject()
   {
      return thrown;
   }

   void printOn(std::ostream &out=std::cout) const
   {
      out << "<FlowException " << thrown << " >";
   }
};

}//namespace FD

#endif /* ifdef _FLOW_EXCEPTION_H */
