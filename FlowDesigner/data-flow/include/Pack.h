// Copyright (C) 1999 Jean-Marc Valin

#ifndef PACK_H
#define PACK_H

#include "Node.h"
#include "ObjectRef.h"

namespace FD {

class Pack : public Node {

protected:
   
   /**The ID of the 'output' output*/
   int outputID;

   /**The ID of the 'stream' input*/
   int inputID;

   /**Reference to the output*/
   ObjectRef output;

   int processCount;

public:
   /**Constructor, takes the name of the node and a set of parameters*/
   Pack(std::string nodeName, ParameterSet params);

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
   Pack() {throw new GeneralException("Pack copy constructor should not be called",__FILE__,__LINE__);}

};
}//namespace FD
#endif
