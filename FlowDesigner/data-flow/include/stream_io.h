// Copyright (C) 2001 Jean-Marc Valin (valj01@gel.usherb.ca)

#ifndef STREAM_IO_H
#define STREAM_IO_H

#include "Object.h"

class EOFObject;
DECLARE_TYPE(EOFObject)

class EOFObject : public Object {
  public:
   void printOn(ostream &out = cout) {out << "<EOFObject >\n";}
};


#endif
