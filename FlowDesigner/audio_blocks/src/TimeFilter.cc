// Copyright (C) 1999 Jean-Marc Valin
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2, or (at your option)
// any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this file.  If not, write to the Free Software Foundation,
// 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

#include <stream.h>
#include "FrameOperation.h"
#include "Buffer.h"
#include "Vector.h"
#include <strstream.h>

class TimeFilter;

DECLARE_NODE(TimeFilter)
/*Node
 *
 * @name TimeFilter
 * @category Signal:DSP
 * @description No description available
 *
 * @input_name INPUT
 * @input_description No description available
 *
 * @output_name OUTPUT
 * @output_description No description available
 *
 * @parameter_name LENGTH
 * @parameter_description No description available
 *
 * @parameter_name FIR
 * @parameter_description No description available
 *
 * @parameter_name IIR
 * @parameter_description No description available
 *
 * @parameter_name LOOKAHEAD
 * @parameter_description No description available
 *
END*/


class TimeFilter : public FrameOperation {
   
   int inputID;
   int inputLength;

   int lookAhead;
   vector<float> fir;
   vector<float> iir;
   //bool first_use;

public:
   TimeFilter(string nodeName, ParameterSet params)
      : FrameOperation(nodeName, params)

   {
      inputID = addInput("INPUT");
      if (parameters.exist("INPUTLENGTH"))
         inputLength = dereference_cast<int> (parameters.get("INPUTLENGTH"));
      else inputLength = dereference_cast<int> (parameters.get("LENGTH"));


      istrstream fir_str(object_cast <String> (parameters.get("FIR")).c_str());
      istrstream iir_str(object_cast <String> (parameters.get("IIR")).c_str());
      fir_str >> fir;
      iir_str >> iir;
      
      if (parameters.exist("LOOKAHEAD"))
         inputsCache[inputID].lookAhead = dereference_cast<int> (parameters.get("LOOKAHEAD"));
      inputsCache[inputID].lookBack = fir.size() - 1 - inputsCache[inputID].lookAhead;
      /*if (iir.size() - 1 > outputs[outputID].lookBack)
         outputs[outputID].lookBack = iir.size() - 1;
      */
      
   }

   ~TimeFilter() {}

   virtual void specificInitialize()
   {
      outputs[outputID].lookBack += iir.size() - 1;
      this->FrameOperation::specificInitialize();
      
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      Vector<float> &output = object_cast<Vector<float> > (out[count]);
      /*if (count < inputsCache[inputID].lookBack)
      {
         output.status = Object::before_beginning;
         return;
	 }*/
      NodeInput input = inputs[inputID];

      int i,j;
      
      for (j=0;j<outputLength;j++)
         output[j] = 0.0;
      //int fir_limit = min(fir.size() - 1, count + inputsCache[inputID].lookAhead + 1 - fir.size());
      int fir_limit = fir.size() - 1;
      for (i = 0; i <= fir_limit ; i++)
      {
	 if (count - i + inputsCache[inputID].lookAhead < 0)
	    break;
         ObjectRef inputValue = input.node->getOutput(input.outputID, count - i + inputsCache[inputID].lookAhead);
         //cerr << "inputsCache[inputID].lookAhead = " << inputsCache[inputID].lookAhead << endl;
         if (inputValue->status != Object::valid)
            continue;

         const Vector<float> &firRow = object_cast<Vector<float> > (inputValue);
         for (j = 0; j < outputLength ; j++)
            output[j] += fir[i]*firRow[j];
      }

      int iir_limit = min(iir.size() - 1, count + inputsCache[inputID].lookAhead + 1 - fir.size());
      for (i = 1; i <= iir_limit ; i++)
      {
         ObjectRef inputValue = this->getOutput(outputID, count - i);
         if (inputValue->status != Object::valid)
         {
            break;
         }
         const Vector<float> &iirRow = object_cast<Vector<float> > (inputValue);
         for (j = 0; j < outputLength ; j++)
            output[j] -= iir[i]*iirRow[j];
      }
      



      output.status = Object::valid;
   }

};
