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

#ifndef LOAD_H
#define LOAD_H

#include "Node.h"
#include "ObjectRef.h"
#include "ObjectParser.h"

class Load;
//DECLARE_NODE(Load)
NODE_INFO(Load, "IO", "STREAM", "OUTPUT", "")

class Load : public Node {

protected:
   
   /**The ID of the 'output' output*/
   int outputID;

   /**The ID of the 'stream' input*/
   int streamInputID;

   /**Reference to the opened stream*/
   ObjectRef currentObject;

public:
   /**Constructor, takes the name of the node and a set of parameters*/
   Load(string nodeName, ParameterSet params);

   /**Class specific initialization routine.
      Each class will call its subclass specificInitialize() method*/
   virtual void specificInitialize();

   /**Class reset routine.
      Each class will call its superclass reset() method*/
   virtual void reset() {this->Node::reset();} 

   /**Ask for the node's output which ID (number) is output_id 
      and for the 'count' iteration */
   virtual ObjectRef getOutput(int output_id, int count); 

protected:
   /**Default constructor, should not be used*/
   Load() {throw new GeneralException("Load copy constructor should not be called",__FILE__,__LINE__);}

};

inline Load::Load(string nodeName, ParameterSet params) 
   : Node(nodeName, params)
{
   outputID = addOutput("OUTPUT");
   streamInputID = addInput("STREAM");
}

inline void Load::specificInitialize()
{
   this->Node::specificInitialize();
}

inline ObjectRef Load::getOutput(int output_id, int count)
{
   if (output_id==outputID)
   {
      if (count != processCount)
      {
         NodeInput streamInput = inputs[streamInputID];
         ObjectRef streamRef = streamInput.node->getOutput(streamInput.outputID,count);
         IStream &stream = object_cast<IStream> (streamRef);
         
         //T *loadedObject = new T;
         //stream >> *loadedObject;
         //currentObject = ObjectRef(loadedObject);
	 cerr << "reading for count = " << count << endl;
	 try {
	    stream >> currentObject;
	 } catch (...)
	 {
	    cerr << "end!\n";
	    return Object::past_endObject;
	 }
	 if (stream.eof() || count == 2)
	    return Object::past_endObject;
         processCount = count;
      }
      return currentObject;
   }
   else 
      throw new NodeException (this, "Load: Unknown output id", __FILE__, __LINE__);
}


#endif
