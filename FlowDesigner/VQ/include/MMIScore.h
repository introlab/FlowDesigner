// Copyright (C) 1999 Jean-Marc Valin
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2, or (at your option)
// any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this file.  If not, write to the Free Software Foundation,
// 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

#ifndef MMISCORE_H
#define MMISCORE_H

#include "Node.h"
#include "ObjectRef.h"

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

public:
   /**Constructor, takes the name of the node and a set of parameters*/
   MMIScore(string nodeName, ParameterSet params);

   /**Class specific initialization routine.
      Each class will call its superclass specificInitialize() method*/
   virtual void specificInitialize();

   virtual void MMIScore::request(int outputID, const ParameterSet &req);

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

#endif
