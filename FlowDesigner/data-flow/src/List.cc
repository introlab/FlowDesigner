// Copyright (C) 1999 Jean-Marc Valin

#include "net_types.h"
#include "Vector.h"
#include "BufferedNode.h"
#include "Stream.h"

using namespace std;

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
 * @output_type Vector<ObjectRef>
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
      
      IStream &file = object_cast<IStream> (inputValue);
      
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



