// Copyright (C) 1999 Jean-Marc Valin

#ifndef _VARIABLES_H_
#define _VARIABLES_H_

#include <string>
#include <map>
#include "Exception.h"
#include "ObjectRef.h"
#include "rc_ptrs.h"

namespace FD {

class Variable {
  public:
   static std::map<std::string, ObjectRef> all;   
};

}//namespace FD

#endif
