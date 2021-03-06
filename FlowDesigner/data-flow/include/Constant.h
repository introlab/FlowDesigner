// Copyright (C) 1999 Jean-Marc Valin & Dominic Letourneau

#ifndef _CONSTANT_NODE_H_
#define _CONSTANT_NODE_H_
#include "Node.h"

namespace FD {

/** A constant node contains a value that will never changes. */
class Constant : public Node
{

protected:

   /**The value of the constant*/
   ObjectRef value;

   /**The ID of the 'value' output*/
   int outputID;
public:

   /**Constructor, takes the name of the node and a set of parameters*/
   Constant(std::string nodeName, ParameterSet params);

   /**Class specific initialization routine.
      Each class will call its subclass initialize() method*/
   virtual void initialize();

   /**Class reset routine.
      Each class will call its superclass reset() method*/
   virtual void reset();

   /**Ask for the node's output which ID (number) is output_id 
      and for the 'count' iteration */
   virtual ObjectRef getOutput(int output_id, int count); 

protected:
   /**Default constructor, should not be used*/
   Constant() {throw new GeneralException("Constant copy constructor should not be called",__FILE__,__LINE__);}

};

}//namespace FD
#endif
