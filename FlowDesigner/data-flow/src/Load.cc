// Copyright (C) 1999 Jean-Marc Valin

#include "BufferedNode.h"
#include "ObjectRef.h"
#include "ObjectParser.h"

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
      if (streamRef->valid != Object::valid)
      {
	 out[count] = streamRef;
	 return;
      }

      Stream &stream = object_cast<Stream> (streamRef);
      
      try {
	 ObjectRef obj;
	 stream >> obj;
	 out[count] = obj;
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
