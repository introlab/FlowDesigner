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

#include "Node.h"
#include "ObjectRef.h"
#include "Vector.h"

#include <unistd.h>

class FDSaveFrame;

//DECLARE_NODE(FDSaveFrame);
NODE_INFO(FDSaveFrame,"Signal:Audio", "OBJECT:FD", "OUTPUT", "LEAD_IN")

class FDSaveFrame : public Node {

protected:
   
   /**The ID of the 'output' output*/
   int outputID;

   /**The ID of the 'stream' input*/
   int streamInputID;

   /**The ID of the 'object' input*/
   int objectInputID;

   /**Reference to the opened stream*/
   ObjectRef openedFile;

public:
   /**Constructor, takes the name of the node and a set of parameters*/
   FDSaveFrame(string nodeName, ParameterSet params) 
      : Node(nodeName, params)
   {
      outputID = addOutput("OUTPUT");
      streamInputID = addInput("FD");
      objectInputID = addInput("OBJECT");
   }
   

   /**Class specific initialization routine.
      Each class will call its subclass specificInitialize() method*/
   virtual void specificInitialize()
   {
      this->Node::specificInitialize();
   }

   /**Class reset routine.
      Each class will call its superclass reset() method*/
   virtual void reset()
   {
      this->Node::reset();
   }

   /**Ask for the node's output which ID (number) is output_id 
      and for the 'count' iteration */
   virtual ObjectRef getOutput(int output_id, int count)
   {
      if (output_id==outputID)
      {
         if (count != processCount)
         {
            NodeInput streamInput = inputs[streamInputID];
            int stream = dereference_cast<int> (streamInput.node->getOutput(streamInput.outputID,count));
            NodeInput objectInput = inputs[objectInputID];
            ObjectRef inputValue = objectInput.node->getOutput(objectInput.outputID,count);
            if (inputValue->status != Object::valid)
               return ObjectRef (new Object(inputValue->status));
            //Vector<unsigned char> &v = object_cast<Vector<unsigned char> > (inputValue);
            //stream.write (v.begin(), v.size());
            //cerr << "stream: " << &stream << endl;
            /*inputValue->rawWrite(stream);
	      stream.flush();*/
	    Vector<float> &vec = object_cast<Vector<float> > (inputValue);
	    short buff[vec.size()];

	    if (count == 0 && parameters.exist("LEAD_IN"))
	    {
	       cerr << "lead\n";
	       for (int i=0;i<vec.size();i++)
		  buff[i]=0;
	       write(stream, (const char *) buff, sizeof(short)*vec.size());

	    }

	    for (int i=0;i<vec.size();i++)
	       buff[i]=vec[i];
	    write(stream, (const char *) buff, sizeof(short)*vec.size());
            processCount=count;
         }
         return ObjectRef(Object::nilObject);
         //return ObjectRef(new Object(Object::nil));
      }
      else 
         throw new NodeException (this, "Save: Unknown output id", __FILE__, __LINE__);
   }
protected:
   /**Default constructor, should not be used*/
   FDSaveFrame() {throw new GeneralException("FDSaveFrame copy constructor should not be called",__FILE__,__LINE__);}

};
