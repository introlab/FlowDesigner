// Copyright (C) 2005 Dominic Letourneau

#include "BufferedNode.h"
#include "ObjectRef.h"
#include "Stream.h"

using namespace std;

namespace FD {

class ReadString;
DECLARE_NODE(ReadString)
/*Node
 *
 * @name ReadString
 * @category IO
 * @description Read a string from file
 *
 * @input_name STREAM
 * @input_description The stream we are loading from
 * @input_type Stream
 *
 * @output_name OUTPUT
 * @output_type string
 * @output_description The (next) string in the stream
 *
END*/


class ReadString : public BufferedNode {

protected:
   
   /**The ID of the 'output' output*/
   int outputID;

   /**The ID of the 'stream' input*/
   int streamInputID;

public:
   ReadString(string nodeName, ParameterSet params) 
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

      string s;
      stream >> s;
      out[count] = ObjectRef(new String(s));

      if (stream.eof())
      {
	 //FIXME: Should have an EOF output too
	 //cerr << "end!\n";
	 out[count] =  nilObject;
      }         
   }

};

}//namespace FD
