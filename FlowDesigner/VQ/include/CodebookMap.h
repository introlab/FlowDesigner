// Copyright (C) 1999 Jean-Marc Valin

#ifndef CODEBOOKMAP_H
#define CODEBOOKMAP_H

#include <math.h>
#include <vector>
#include <iostream>
#include "Object.h"
#include "vq.h"
#include "Vector.h"

class CodebookMap;

std::ostream &operator << (std::ostream &out, const CodebookMap &cell);


class CodebookMap : public Object {
protected:
   RCPtr<VQ> mapIn;
   
   Vector<Vector<float> > mapOut;

public:
   //CodebookMap() 
   //{}

   CodebookMap(){}
   
   CodebookMap (const CodebookMap &) {std::cerr << "don't call the CodebookMap copy constructor\n"; exit(1);}

   CodebookMap(const RCPtr<VQ> &_mapIn, const std::vector<float *> dataIn, const std::vector<float *> dataOut, int length);

   ~CodebookMap()
   {
   }

   
   const float * calcOutput(const float *in) const;

   void printOn(std::ostream &out) const;

   void readFrom (std::istream &in);

   friend std::istream &operator >> (std::istream &in, CodebookMap &cell);
};

#endif
