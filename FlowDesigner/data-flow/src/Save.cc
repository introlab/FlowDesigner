// Copyright (C) 1999 Jean-Marc Valin

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

      OStream &stream = object_cast<OStream> (streamValue);
      
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



