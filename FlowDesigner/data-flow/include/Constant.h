// Copyright (C) 1999 Jean-Marc Valin & Dominic Letourneau

#ifndef _CONSTANT_NODE_H_
#define _CONSTANT_NODE_H_


#include "Node.h"

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
   Constant(string nodeName, ParameterSet params);

   /**Do nothing for requests since we have no inputs*/
   virtual void request(int outputID, const ParameterSet &req) {}

   /**Class specific initialization routine.
      Each class will call its subclass specificInitialize() method*/
   virtual void specificInitialize();

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

#endif
