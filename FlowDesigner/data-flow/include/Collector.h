// Copyright (C) 1999 Jean-Marc Valin & Dominic Letourneau

#ifndef Collector_h
#define Collector_h

#include "Node.h"
#include "Exception.h"
#include <map>

class Collector : public Node
{
 
public:

   ///Constructor, takes the name of the node and a set of parameters
   Collector(string nodeName, ParameterSet params);

   /**Class specific initialization routine.
      Each class will call its subclass specificInitialize() method*/
   virtual void specificInitialize();

   /**Ask for the node's output which ID (number) is output_id 
      and for the 'count' iteration */
   virtual ObjectRef getOutput(int output_id, int count); 
  
   /**Ask for the node's output (named) and for the count iteration */
   virtual ObjectRef getOutputNamed (const string &outputName, int count);
   

   ///Checks whether node really has a certain output
   virtual bool hasOutput(int output_id) const;

   virtual void request(int outputID, const ParameterSet &req) {inputs[outputID].node->request(inputs[outputID].outputID,req);}

protected:
   ///symbolic to numeric translation for input names
   virtual int translateInput(string inputName);

   ///symbolic to numeric translation for output names
   virtual int translateOutput(string inputName);

protected:
   ///Default constructor, should not be used
   Collector() {throw new GeneralException("Collector default constructor should not be called",__FILE__,__LINE__);}

};

#endif
