// Copyright (C) 1999 Jean-Marc Valin

#ifndef GMMSCORE_H
#define GMMSCORE_H

#include "Node.h"
#include "ObjectRef.h"

class GMMScore : public Node {

protected:
   
   /**The ID of the 'output' output*/
   int outputID;

   /**The ID of the 'stream' input*/
   int gmmInputID;

   /**The ID of the 'frames' input*/
   int framesInputID;

   /**Reference to the current stream*/
   ObjectRef currentScore;

   int processCount;

public:
   /**Constructor, takes the name of the node and a set of parameters*/
   GMMScore(string nodeName, ParameterSet params);

   /**Standard request-passing method between nodes during initialization*/
   virtual void request(int outputID, const ParameterSet &req)
   {
      inputs[gmmInputID].node->request(inputs[gmmInputID].outputID,req);
      inputs[framesInputID].node->request(inputs[framesInputID].outputID,req);
   }

   /**Class specific initialization routine.
      Each class will call its superclass specificInitialize() method*/
   virtual void specificInitialize();

   /**Class reset routine.
      Each class will call its superclass reset() method*/
   virtual void reset();

   /**Ask for the node's output which ID (number) is output_id 
      and for the 'count' iteration */
   virtual ObjectRef getOutput(int output_id, int count); 

protected:
   /**Default constructor, should not be used*/
   GMMScore() {throw new GeneralException("GMMScore copy constructor should not be called",__FILE__,__LINE__);}

};

#endif
