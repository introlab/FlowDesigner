// Copyright (C) 1999 Jean-Marc Valin

#include "BufferedNode.h"
#include "ObjectRef.h"
#include "ObjectParser.h"
#include "Stream.h"

using namespace std;

class Load;
DECLARE_NODE(Load)
/*Node
 *
 * @name Load
 * @category IO
 * @description Load an object from file (registered type)
 *
 * @input_name STREAM
 * @input_description The stream we are loading from
 * @input_type Stream
 *
 * @output_name OUTPUT
 * @output_description The loaded object
 *
END*/


class Load : public BufferedNode {

protected:
   
   /**The ID of the 'output' output*/
   int outputID;

   /**The ID of the 'stream' input*/
   int streamInputID;

public:
   Load(string nodeName, ParameterSet params) 
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
      
      ObjectRef obj;
      stream >> obj;
      out[count] = obj;
   }

};
