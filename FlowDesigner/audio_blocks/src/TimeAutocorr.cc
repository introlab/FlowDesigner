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

#include "BufferedNode.h"
#include "Buffer.h"
#include "Vector.h"
#include <strstream>
#include <values.h>
#include "vec.h"

class TimeAutocorr;

DECLARE_NODE(TimeAutocorr)
/*Node
 *
 * @name TimeAutocorr
 * @category Signal:DSP
 * @description No description available
 *
 * @input_name INPUT
 * @input_description No description available
 *
 * @output_name OUTPUT
 * @output_description No description available
 *
 * @parameter_name INPUTLENGTH
 * @parameter_description No description available
 *
 * @parameter_name LOOKAHEAD
 * @parameter_description No description available
 *
 * @parameter_name LOOKBACK
 * @parameter_description No description available
 *
END*/


class TimeAutocorr : public BufferedNode {
   
   int inputID;
   int outputID;

   int inputLength;
   int outputLength;

   int numberFrames;
   vector<Vector<float> *> frames;

public:
   TimeAutocorr(string nodeName, ParameterSet params)
      : BufferedNode(nodeName, params)

   {
      inputID = addInput("INPUT");
      outputID = addOutput("OUTPUT");

      inputLength = dereference_cast<int> (parameters.get("INPUTLENGTH"));
      
      inputsCache[inputID].lookAhead = dereference_cast<int> (parameters.get("LOOKAHEAD"));
      inputsCache[inputID].lookBack = dereference_cast<int> (parameters.get("LOOKBACK"));

      numberFrames=inputsCache[inputID].lookBack+inputsCache[inputID].lookAhead+1;
      frames.resize(numberFrames);
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      NodeInput input = inputs[inputID];

      if (count < inputsCache[inputID].lookBack)
      {
         out[count] = Object::before_beginningObject;
         return;
      }
      Vector<float> &output = *Vector<float>::alloc(numberFrames);
      out[count] = &output;

      int i,j;

      vector <RCPtr<Vector<float> > > inVect;
      for (i = -inputsCache[inputID].lookBack, j=0; i <= inputsCache[inputID].lookAhead ; i++, j++)
      {
         ObjectRef inputValue = input.node->getOutput(input.outputID, count + i);

         if (inputValue->status != Object::valid)
         {
	    out[count] = inputValue;
            return;
         }
	 
	 inVect.insert(inVect.end(),RCPtr<Vector<float> > (inputValue));
      }      
      
      for (int i=0;i<output.size();i++)
      {
	 output[i] = vec_inner_prod(&(*inVect[0])[0], &(*inVect[i])[0], inputLength);
      }
   }

};
