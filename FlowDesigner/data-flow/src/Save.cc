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

#include "Save.h"
#include "net_types.h"
#include "Object.h"

#include <iostream>
using namespace std;

DECLARE_NODE(Save)
/*Node
 *
 * @name Save
 * @category IO
 * @description Takes an object and saves it using a stream, returns the input object
 *
 * @input_name OBJECT
 * @input_description The object that will be saved
 *
 * @input_name STREAM
 * @input_description The output stream where to save
 * @input_type Stream
 *
 * @output_name OUTPUT
 * @output_description The input object
 *
END*/


Save::Save(string nodeName, ParameterSet params) 
   : Node(nodeName, params)
{
   outputID = addOutput("OUTPUT");
   streamInputID = addInput("STREAM");
   objectInputID = addInput("OBJECT");
}

void Save::specificInitialize()
{
   this->Node::specificInitialize();
}

void Save::reset()
{
   this->Node::reset();
}

ObjectRef Save::getOutput(int output_id, int count)
{
   if (output_id==outputID)
   {
      if (count > processCount)
      {
	 ObjectRef objectValue;
	 for (int i = processCount+1 ; i<=count; i++)
	 {
	    NodeInput objectInput = inputs[objectInputID];
	    objectValue = objectInput.node->getOutput(objectInput.outputID,i);
	    Object &object = *objectValue;
	    
	    NodeInput streamInput = inputs[streamInputID];
	    ObjectRef streamValue = streamInput.node->getOutput(streamInput.outputID,i);
	    Stream &stream = object_cast<Stream> (streamValue);
	    
	    //stream << object << endl;
            //FIXME: this is a kludge
	    stream << object;
            ostream &tmp = stream;
	    tmp << endl;

	    stream.flush();
	    
	 }
	 processCount = count;
	 return objectValue;
      } else {
         NodeInput objectInput = inputs[objectInputID];
         ObjectRef objectValue = objectInput.node->getOutput(objectInput.outputID,count);
         return objectValue;         
      }
      //return ObjectRef(new Object(Object::nil));
   }
   else 
      throw new NodeException (this, "Save: Unknown output id", __FILE__, __LINE__);
}
