// Copyright (C) 1999 Jean-Marc Valin

#ifndef UNPACK_H
#define UNPACK_H

#include "Node.h"
#include "ObjectRef.h"

namespace FD {

class UnPack : public Node {

protected:
   
   /**The ID of the 'output' output*/
   int outputID;

   /**The ID of the 'NOT_END' output*/
   int endID;

   /**The ID of the 'stream' input*/
   int inputID;

   int processCount;

public:
   /**Constructor, takes the name of the node and a set of parameters*/
   UnPack(std::string nodeName, ParameterSet params);

   /**Class specific initialization routine.
      Each class will call its superclass initialize() method*/
   virtual void initialize();

   /**Class reset routine.
      Each class will call its superclass reset() method*/
   virtual void reset();

   /**Ask for the node's output which ID (number) is output_id 
      and for the 'count' iteration */
   virtual ObjectRef getOutput(int output_id, int count); 

protected:
   /**Default constructor, should not be used*/
   UnPack() {throw new GeneralException("UnPack copy constructor should not be called",__FILE__,__LINE__);}

};
}//namespace FD
#endif
