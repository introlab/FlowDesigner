// Copyright (C) 1999 Jean-Marc Valin

#ifndef MMISCORE_H
#define MMISCORE_H

#include "Node.h"
#include "ObjectRef.h"

namespace FD {

class MMIScore : public Node {

protected:
   
   /**The ID of the 'output' output*/
   int outputID;

   /**The ID of the 'stream' input*/
   int mmiInputID;

   /**The ID of the 'frames' input*/
   int framesInputID;

   /**Reference to the current stream*/
   ObjectRef currentScore;

   int processCount;

public:
   /**Constructor, takes the name of the node and a set of parameters*/
   MMIScore(std::string nodeName, ParameterSet params);

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
   MMIScore() {throw new GeneralException("MMIScore copy constructor should not be called",__FILE__,__LINE__);}

};

}//namespace FD
#endif
