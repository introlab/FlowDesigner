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

#include "BufferedNode.h"
#include "net_types.h"
#include "Object.h"

#include <iostream>
using namespace std;

class Save;
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

class Save : public BufferedNode {

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
   Save(string nodeName, ParameterSet params) 
      : BufferedNode(nodeName, params)
   {
      outputID = addOutput("OUTPUT");
      streamInputID = addInput("STREAM");
      objectInputID = addInput("OBJECT");
   }


   void calculate(int output_id, int count, Buffer &out)
   {
      ObjectRef objectValue = getInput(objectInputID,count);
      Object &object = *objectValue;
      
      ObjectRef streamValue = getInput(streamInputID,count);
      if (streamValue->valid != Object::valid)
      {
	 out[count] = streamValue;
	 return;
      }

      Stream &stream = object_cast<Stream> (streamValue);
      
      stream << object;
      ostream &tmp = stream;
      tmp << endl;
      stream.flush();
      out[count] = objectValue;
   }

   virtual void request(int outputID, const ParameterSet &req) 
   {
      inputs[objectInputID].node->request(outputID,req);
   }


};



