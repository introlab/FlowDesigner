// Copyright (C) 1999 Jean-Marc Valin

#include "BufferedNode.h"
#include "Buffer.h"
#include "Vector.h"
#include <math.h>

using namespace std;

namespace FD {

class Reframe;

DECLARE_NODE(Reframe)
/*Node
 *
 * @name Reframe
 * @category DSP:Manip
 * @description Applies a window on a frame
 *
 * @input_name INPUT
 * @input_type Vector<float>
 * @input_description Input frame
 *
 * @output_name OUTPUT
 * @output_type Vector<float>
 * @output_description Reframeed frame
 *
 * @parameter_name LENGTH
 * @parameter_type int
 * @parameter_description Length of the frames
 *
 * @parameter_name ADVANCE
 * @parameter_type int
 * @parameter_description Frame advance (offset)
 *
END*/


class Reframe : public BufferedNode {
   
   int inputID;
   int outputID;
   int length;
   int advance;
   Vector<float> buff;
   int lastPos;
   int currentCount;
public:
   Reframe(string nodeName, ParameterSet params)
      : BufferedNode(nodeName, params)
   {
      inputID = addInput("INPUT");
      outputID = addOutput("OUTPUT");
      length = dereference_cast<int> (parameters.get("LENGTH"));
      advance = dereference_cast<int> (parameters.get("ADVANCE"));
      
      inputsCache[inputID].lookBack=1;
      inputsCache[inputID].lookAhead=1;
      lastPos=0;
      currentCount=0;
      inOrder = true;
   }

   void reset()
   {
      lastPos = 0;
      currentCount = 0;
      BufferedNode::reset();
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      Vector<float> &output = *Vector<float>::alloc(length);
      out[count] = &output;

      //How many can we copy from the buffer
      int nbCopy = min(lastPos, length);

      //cerr << nbCopy << endl;

      //Copying from the buffer
      for (int i=0;i<nbCopy;i++)
	 output[i] = buff[i];
      //How many copied so far
      int fill = nbCopy;
      //updating the buffer
      lastPos -= advance;
      
      for (int i=0;i<lastPos;i++)
	 buff[i] = buff[i+advance];

      while (fill != output.size())
      {
	 ObjectRef inputValue = getInput(inputID, currentCount++);
	 const Vector<float> &in = object_cast<Vector<float> > (inputValue);
	 int inputLength = in.size();
	 int newPos = lastPos+inputLength;

	 //cerr << newPos << endl;

	 if (newPos > buff.size())
	    buff.resize(newPos);
	 int buffDiscard=0;
	 if (lastPos < 0)
	    buffDiscard = -lastPos;
	 for (int i=buffDiscard;i<inputLength;i++)
	    buff[i+lastPos] = in[i];

	 nbCopy=min(inputLength, length-fill);

	 //cerr << nbCopy << endl;
	 for (int i=0;i<nbCopy;i++)
	    output[i+fill] = in[i];
	 fill += nbCopy;
	 lastPos = newPos;
      }
      if (lastPos <0)
	 lastPos = 0;

   }

};

}//namespace FD
