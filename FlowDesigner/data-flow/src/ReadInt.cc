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
#include "ObjectRef.h"

class ReadInt;
DECLARE_NODE(ReadInt)
/*Node
 *
 * @name ReadInt
 * @category IO
 * @description ReadInt an integer from file
 *
 * @input_name STREAM
 * @input_description The stream we are loading from
 * @input_type Stream
 *
 * @output_name OUTPUT
 * @output_type int
 * @output_description The (next) integer in the stream
 *
END*/


class ReadInt : public BufferedNode {

protected:
   
   /**The ID of the 'output' output*/
   int outputID;

   /**The ID of the 'stream' input*/
   int streamInputID;

public:
   ReadInt(string nodeName, ParameterSet params) 
      : BufferedNode(nodeName, params)
   {
      outputID = addOutput("OUTPUT");
      streamInputID = addInput("STREAM");
      inOrder = true;
   }



   void calculate(int output_id, int count, Buffer &out)
   {
      ObjectRef streamRef = getInput(streamInputID,count);
      if (streamRef->valid != Object::valid)
      {
	 out[count] = streamRef;
	 return;
      }

      Stream &stream = object_cast<Stream> (streamRef);
      int i;
      try {
	 stream >> i;
	 out[count] = new Int(i);
      } catch (BaseException *e)
      {
	 //cerr << "base exception\n";
	 //e->print();
	 out[count] = Object::past_endObject;
      } catch (...)
      {
	 //cerr << "nil!\n";
	 out[count] =  Object::past_endObject;
      }
      if (stream.eof())
      {
	 //cerr << "end!\n";
	 out[count] =  Object::past_endObject;
      }         
   }

};
