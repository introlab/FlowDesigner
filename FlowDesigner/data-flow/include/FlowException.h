// Copyright (C) 1999 Jean-Marc Valin

#ifndef _FLOW_EXCEPTION_H
#define _FLOW_EXCEPTION_H

#include "Object.h"
#include "ObjectRef.h"

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
};

#endif /* ifdef _FLOW_EXCEPTION_H */
