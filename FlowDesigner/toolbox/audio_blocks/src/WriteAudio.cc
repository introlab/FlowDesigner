// Copyright (C) 1999 Jean-Marc Valin

#include "BufferedNode.h"
#include "ObjectRef.h"
#include "Vector.h"

#include <unistd.h>
#include <math.h>
#include "Stream.h"

using namespace std;

namespace FD {

class WriteAudio;

DECLARE_NODE(WriteAudio)
/*Node
 *
 * @name WriteAudio
 * @category DSP:Audio
 * @description Writes audio frames to the sound card (or any other) stream
 *
 * @input_name OBJECT
 * @input_type Vector<float>
 * @input_description Audio frames
 *
 * @input_name DEVICE
 * @input_type Stream
 * @input_description (Sound card) stream
 *
 * @output_name OUTPUT
 * @output_type Vector<float>
 * @output_description Returning the input audio frames
 *
 * @parameter_name SAMPLE_SIZE
 * @parameter_type int
 * @parameter_value 16
 * @parameter_description Number of bits/sample
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

   int sampleSize; // in bytes/sample
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
      sampleSize = dereference_cast<int> (parameters.get("SAMPLE_SIZE")) / 8; // bits/sample to bytes/sample
      if(sampleSize == 0) {
        sampleSize = 1;
        std::cerr << "WriteAudio : SAMPLE_SIZE must be greater than 7. Using 8 instead..." << std::endl;
      }
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
      char buff[sampleSize*vec.size()];
      
      if (count == 0)
      {
	 
	 for (int i=0;i<sampleSize*vec.size();i++)
	    buff[i]=0;
	 for (int i=0;i<lead;i++)
	    stream.write((const char *)buff, sampleSize*vec.size());
      }

     char cVal;
     short sVal;
     int iVal;        
     for (int i=0;i<vec.size();i++) {
        iVal = rint(vec[i]);
        if(sampleSize == 1) {
            cVal = char(iVal);
            memcpy(buff + (i*sampleSize), &cVal, sampleSize);
        }
        else if(sampleSize == 2) {
            sVal = short(iVal);
            memcpy(buff + (i*sampleSize), &sVal, sampleSize);
        }
        else if(sampleSize == 4) {
            memcpy(buff + (i*sampleSize), &iVal, sampleSize);
        }
     }
      stream.write((const char *)buff, sampleSize*vec.size());

      out[count] = inputValue;
   }
};

}//namespace FD
