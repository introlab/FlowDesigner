// Copyright (C) 1999 Jean-Marc Valin

#include "BufferedNode.h"
#include "ObjectRef.h"
#include "Vector.h"

#include <unistd.h>
#include <math.h>

using namespace std;

class FDSaveFrame;

DECLARE_NODE(FDSaveFrame)
/*Node
 *
 * @name FDSaveFrame
 * @category ZDeprecated
 * @description Writes audio frames to the sound card (or any other) file descriptor (deplaced by WriteAudio)
 *
 * @input_name OBJECT
 * @input_type Vector<float>
 * @input_description Audio frames
 *
 * @input_name FD
 * @input_type FILEDES
 * @input_description (Sound card) File descriptor
 *
 * @output_name OUTPUT
 * @output_type Vector<float>
 * @output_description Returning the input audio frames
 *
 * @parameter_name LEAD_IN
 * @parameter_type int
 * @parameter_description Number of zero frames to send before starting (for synchronization)
 *
END*/


class FDSaveFrame : public BufferedNode {

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
   FDSaveFrame(string nodeName, ParameterSet params) 
      : BufferedNode(nodeName, params)
   {
      outputID = addOutput("OUTPUT");
      streamInputID = addInput("FD");
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
      
      int stream = dereference_cast<int> (streamValue);

      Vector<float> &vec = object_cast<Vector<float> > (inputValue);
      short buff[vec.size()];
      
      if (count == 0)
      {
	 
	 for (int i=0;i<vec.size();i++)
	    buff[i]=0;
	 for (int i=0;i<lead;i++)
	    write(stream, (const char *) buff, sizeof(short)*vec.size());
	 
      }
      
      for (int i=0;i<vec.size();i++)
	 buff[i]= short(rint(vec[i]));
      write(stream, (const char *) buff, sizeof(short)*vec.size());

      out[count] = inputValue;
   }
};
