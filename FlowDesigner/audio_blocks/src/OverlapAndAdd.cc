// Copyright (C) 1999 Jean-Marc Valin

#include "BufferedNode.h"
#include "Buffer.h"
#include "Vector.h"

class OverlapAndAdd;

DECLARE_NODE(OverlapAndAdd)
/*Node

 * @name OverlapAndAdd
 * @category DSP:Audio
 * @description No description available

 * @input_name INPUT
 * @input_description No description available

 * @output_name OUTPUT
 * @output_description No description available

END*/


class OverlapAndAdd : public BufferedNode {
   
   int inputID;
   int outputID;

public:
   OverlapAndAdd(string nodeName, ParameterSet params)
   : BufferedNode(nodeName, params)
   {
      inputID = addInput("INPUT");
      outputID = addOutput("OUTPUT");
      inputsCache[inputID].lookBack = 1;
      inputsCache[inputID].lookAhead = 1;
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      ObjectRef inputValue = getInput(inputID, count);

      const Vector<float> &in = object_cast<Vector<float> > (inputValue);
      int inputLength = in.size();
      int outputLength = inputLength >> 1;

      Vector<float> &output = *Vector<float>::alloc(outputLength);
      out[count] = &output;


      const Vector<float> *past;
      const Vector<float> *next;
      bool can_look_back = false;
      bool can_look_ahead = false;
      if (count > 0)   
      {
         ObjectRef pastInputValue = getInput(inputID, count-1);
	 can_look_back=true;
	 past = &object_cast<Vector<float> > (pastInputValue);
      }
      
      ObjectRef nextInputValue = getInput(inputID, count+1);
      can_look_ahead=true;
      next = &object_cast<Vector<float> > (nextInputValue);


      int before = (inputLength-outputLength)/2;
      int after = inputLength - outputLength - before;
      int i,j;

      for (int i=0;i<outputLength;i++)
      {
	 output[i]=in[i+before];
      }

      if (can_look_back)
	 for (i=0, j=inputLength-before ; i < before ; i++, j++)
	    output[i]+=(*past)[j];
      

      if (can_look_ahead)
      	 for (i=0, j=outputLength-after ; i < after ; i++, j++)
	    output[j]+=(*next)[i];


  
      /*
      int begin = (inputLength-outputLength)/2;
      int end = outputLength+begin;

      cerr << begin << " " << end << endl;
      int i,j;

      for (i=begin,j=0;i<end;i++,j++)
      {
         output[j]=in[i];
      }

      if (can_look_back)
      {
	 for (i=end,j=0;i<inputLength;i++,j++)
	    output[j] += (*past)[i];
      }

      if (can_look_ahead)
      {
	 for (i=0,j=outputLength-begin;i<begin;i++,j++)
	    output[j] += (*next)[i];
      }
      
      */
   }

};
