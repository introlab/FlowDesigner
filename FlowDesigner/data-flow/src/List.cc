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

#include "net_types.h"
#include "Vector.h"
#include "BufferedNode.h"

class List;
DECLARE_NODE(List)
/*Node
 *
 * @name List
 * @category General
 * @description Load a string from a file (seperated into chunks of 256 bytes)
 *
 * @input_name STREAM
 * @input_description The stream to load from
 * @input_type Stream
 *
 * @output_name OUTPUT
 * @output_description The vector output
 * @output_type Vector
 *
END*/


class List : public BufferedNode {

protected:
   
   int outputID;

   int streamInputID;

public:

   List(string nodeName, ParameterSet params) 
      : BufferedNode(nodeName, params)
   {
      outputID = addOutput("OUTPUT");
      streamInputID = addInput("STREAM");
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      Vector<ObjectRef> *strList = new Vector<ObjectRef>;
      
      NodeInput input = inputs[streamInputID];
      ObjectRef inputValue = input.node->getOutput(input.outputID,count);
      
      Stream &file = object_cast<Stream> (inputValue);
      
      char tmpLine[256];
      while (true)
      {
	 file.getline(tmpLine, 255);
	 if (file.fail()) break;
	 strList->insert(strList->end(), ObjectRef (new String(tmpLine)));
      }

      out[count] = ObjectRef(strList);
   }


protected:
   /**Default constructor, should not be used*/
   List() {throw new GeneralException("List copy constructor should not be called",__FILE__,__LINE__);}

};



