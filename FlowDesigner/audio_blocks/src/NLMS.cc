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
#include <stdlib.h>

class NLMS;

DECLARE_NODE(NLMS)
/*Node
 *
 * @name NLMS
 * @category Signal:DSP
 * @description Normalized LMS algorithm
 *
 * @input_name INPUT
 * @input_description The input of the adaptive FIR filter
 *
 * @input_name REF
 * @input_description The signal being tracked
 *
 * @output_name OUTPUT
 * @output_description The output of the adaptive FIR filter (not the residue)
 *
 * @parameter_name FILTER_LENGTH
 * @parameter_description Length of the adaptive FIR filter
 *
 * @parameter_name ALPHA
 * @parameter_description Adaptation rate of the filter coefficients
 *
 * @parameter_name BETA
 * @parameter_description Adaptation rate of the normalization energy estimate
 *
END*/


class NLMS : public BufferedNode {
   
   int inputID;
   int refID;
   int outputID;
   int size;
   Vector<float> a;
   float alpha;
   float beta;
   float E;
public:
   NLMS(string nodeName, ParameterSet params)
   : BufferedNode(nodeName, params)
   {
      inOrder = true;
      inputID = addInput("INPUT");
      refID = addInput("REF");
      outputID = addOutput("OUTPUT");

      size = dereference_cast<int> (parameters.get("FILTER_LENGTH"));
      alpha = dereference_cast<float> (parameters.get("ALPHA"));
      beta = dereference_cast<float> (parameters.get("BETA"));
      a.resize(size,0.0);
      inputsCache[inputID].lookBack=1;
   }

   void specificInitialize()
   {
      for (int j=0;j<size;j++)
	 a[j] = 1.0/size;
      E=1e-6;
      BufferedNode::specificInitialize();
   }

   void reset()
   {
      BufferedNode::reset();
      for (int j=0;j<size;j++)
	 a[j] = 1.0/size;
      E=1e-6;
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      ObjectRef inputValue = getInput(inputID, count);
      ObjectRef refValue = getInput(refID, count);

      if (inputValue->status != Object::valid)
      {
	 out[count] = inputValue;
         return;
      }

      if (refValue->status != Object::valid)
      {
	 out[count] = refValue;
         return;
      }

      const Vector<float> &in = object_cast<Vector<float> > (inputValue);
      const Vector<float> &ref = object_cast<Vector<float> > (refValue);
      int inputLength = in.size();

      Vector<float> &output = *Vector<float>::alloc(inputLength);
      out[count] = &output;

      for (int i=0;i<inputLength;i++)
         output[i]=0;

      const Vector<float> *past;
      bool can_look_back = false;
      if (count > 0)   
      {
         ObjectRef pastInputValue = getInput(inputID, count-1);
         if (pastInputValue->status == Object::valid)
         {
            can_look_back=true;
            past = &object_cast<Vector<float> > (pastInputValue);
         }      
      }      
      
      float _x[inputLength+size-1];
      float *x=_x+size-1;

      if (can_look_back)
      {
	 for (int i=0;i<size-1;i++)
	 {
	    _x[i]=(*past)[inputLength-size+1+i];
	 }
      } else {
	 for (int i=0;i<size-1;i++)
	 {
	    _x[i]=0.0;
	 }
      }
      for (int i=0;i<inputLength;i++)
      {
	 x[i]=in[i];
      }

      float err, norm;
      for (int i=0;i<inputLength;i++)
      {
	 for (int j=0;j<size;j++)
	    output[i] += a[j]*x[i-j];
	 float err = ref[i]-output[i];
	 
	 E = (1-beta)*E + beta*x[i]*x[i];
	 norm = alpha*err/E;
	 for (int j=0;j<size;j++)
	    a[j] += norm*x[i-j];
      }
      /*for (int j=0;j<size;j++)
	 cout << a[j] << "\t";
	 cout << endl;*/

   }

};
