// Copyright (C) 1999 Jean-Marc Valin

#include "BufferedNode.h"
#include "ObjectRef.h"
#include "Vector.h"

#include <unistd.h>
#include <math.h>

class WriteAudio;

DECLARE_NODE(WriteAudio)
/*Node
 *
 * @name WriteAudio
 * @category DSP:Audio
 * @description Writes audio frames to the sound card (or any other) stream
 *
 * @input_name OBJECT
 * @input_type Vector
 * @input_description Audio frames
 *
 * @input_name DEVICE
 * @input_type Stream
 * @input_description (Sound card) stream
 *
 * @output_name OUTPUT
 * @output_type Vector
 * @output_description Returning the input audio frames
 *
 * @parameter_name LEAD_IN
 * @parameter_type int
 * @parameter_description Number of zero frames to send before starting (for synchronization)
 *
END*/


class WriteAudio : public BufferedNode {

protected:
   
   /**The ID of the 'output' output*/
   int outputID;

   /**The ID of the 'stream' input*/
   int streamInputID;

   /**The ID of the 'object' input*/
   int objectInputID;

   int lead;
public:
   /**Constructor, takes the name of the node and a set of parameters*/
   WriteAudio(string nodeName, ParameterSet params) 
      : BufferedNode(nodeName, params)
   {
      outputID = addOutput("OUTPUT");
      streamInputID = addInput("DEVICE");
      objectInputID = addInput("OBJECT");
      inOrder = true;
      if (parameters.exist("LEAD_IN"))
	 lead = dereference_cast<int> (parameters.get("LEAD_IN"));
      else
	 lead = 0;
   }
   

   void calculate(int output_id, int count, Buffer &out)
   {

      ObjectRef inputValue = getInput(objectInputID,count);


      ObjectRef streamValue = getInput(streamInputID,count);
      
      OStream &stream = object_cast<OStream> (streamValue);

      Vector<float> &vec = object_cast<Vector<float> > (inputValue);
      short buff[vec.size()];
      
      if (count == 0)
      {
	 
	 for (int i=0;i<vec.size();i++)
	    buff[i]=0;
	 for (int i=0;i<lead;i++)
	    stream.write((const char *)buff, sizeof(short)*vec.size());
      }
      
      for (int i=0;i<vec.size();i++)
	 buff[i]= short(rint(vec[i]));
      stream.write((const char *)buff, sizeof(short)*vec.size());

      out[count] = inputValue;
   }
};
