// Copyright (C) 1999 Jean-Marc Valin

#include "BufferedNode.h"
#include "Buffer.h"
#include "Vector.h"
#include <stdlib.h>

class Overlap;

DECLARE_NODE(Overlap)
/*Node
 *
 * @name Overlap
 * @category DSP:Manip
 * @description Outputs overlapping frames from non-overlapping ones
 *
 * @input_name INPUT
 * @input_type Vector<float>
 * @input_description (Non-overlapped) input frames
 *
 * @output_name OUTPUT
 * @output_type Vector<float>
 * @output_description Overlapped output frames
 *
 * @parameter_name OUTPUTLENGTH
 * @parameter_type int
 * @parameter_description Frame length for output overlapped frames
 *
END*/


class Overlap : public BufferedNode {
   
   int inputID;
   int outputID;
   int filterID;
   int outputLength;

public:
   Overlap(string nodeName, ParameterSet params)
   : BufferedNode(nodeName, params)
   {
      inputID = addInput("INPUT");
      outputID = addOutput("OUTPUT");
      outputLength = dereference_cast<int> (parameters.get("OUTPUTLENGTH"));
      inputsCache[inputID].lookAhead=1;
      inputsCache[inputID].lookBack=1;
   }

   virtual void specificInitialize()
   {
      inputsCache[inputID].lookBack=1;
      inputsCache[inputID].lookAhead=1;
      this->BufferedNode::specificInitialize();
      
      
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      ObjectRef inputValue = getInput(inputID, count);

      if (inputValue->status != Object::valid)
      {
	 out[count] = inputValue;
         return;
      }
      const Vector<float> &in = object_cast<Vector<float> > (inputValue);
      int inputLength = in.size();

      Vector<float> &output = *Vector<float>::alloc(outputLength);
      out[count] = &output;

      const Vector<float> *past;
      const Vector<float> *next;
      bool can_look_back = false;
      bool can_look_ahead = false;
      if (count > 0)   
      {
         ObjectRef pastInputValue = getInput(inputID, count-1);
         if (pastInputValue->status == Object::valid)
         {
            can_look_back=true;
            past = &object_cast<Vector<float> > (pastInputValue);
         }      
      }
      
      ObjectRef nextInputValue = getInput(inputID, count+1);
      if (nextInputValue->status == Object::valid)
      {
	 can_look_ahead=true;
	 next = &object_cast<Vector<float> > (nextInputValue);
      }
      
      
      for (int i=0;i<outputLength;i++)
         output[i]=0; 
      
      int before = (outputLength-inputLength)/2;
      int after = outputLength - inputLength - before;
      int i,j;

      //cerr << before << " " << after << endl;

      if (can_look_back)
	 for (i=0, j=inputLength-before ; i < before ; i++, j++)
	    output[i]=(*past)[j];
      

      if (can_look_ahead)
      	 for (i=0, j=outputLength-after ; i < after ; i++, j++)
	    output[j]=(*next)[i];

      for (int i=0;i<inputLength;i++)
      {
	 output[i+before]=in[i];
      }
   }

};
