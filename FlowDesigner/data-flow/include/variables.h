// Copyright (C) 1999 Jean-Marc Valin

#ifndef _VARIABLES_H_
#define _VARIABLES_H_

#include "rc_ptrs.h"
#include <string>
#include <map>
#include "Exception.h"
#include "ObjectRef.h"

class Variable {
  public:
   static std::map<std::string, ObjectRef> all;
   
};


#endif
