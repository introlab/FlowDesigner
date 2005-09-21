// Copyright (C) 1999 Jean-Marc Valin

#include "BufferedNode.h"
#include "ObjectRef.h"
#include "Stream.h"

using namespace std;

namespace FD {

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
      IStream &stream = object_cast<IStream> (streamRef);

      int i;
      stream >> i;
      out[count] = Int::alloc(i);

      if (stream.eof())
      {
	 //FIXME: Should have an EOF output too
	 //cerr << "end!\n";
	 out[count] =  nilObject;
      }         
   }

};

}//namespace FD
