// Copyright (C) 1999 Jean-Marc Valin

#include "BufferedNode.h"
#include "Buffer.h"
#include "Vector.h"
#include <stdlib.h>
#include <math.h>

using namespace std;

namespace FD {

class NLMS;

DECLARE_NODE(NLMS)
/*Node
 *
 * @name NLMS
 * @category DSP:Adaptive
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
 * @parameter_name POWER
 * @parameter_description Normalization power
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
   float power;
      //Vector<float> w;
      //Vector<float> grad;
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
      power = dereference_cast<float> (parameters.get("POWER"));
      a.resize(size,0.0);
      //w.resize(size,1.0);
      //grad.resize(size,0.0);
      inputsCache[inputID].lookBack=1;
   }

   void initialize()
   {
      for (int j=0;j<size;j++)
	 a[j] = 0;//1.0/size;
      E=1e-6;
      BufferedNode::initialize();
   }

   void reset()
   {
      BufferedNode::reset();
      for (int j=0;j<size;j++)
	 a[j] = 0;//1.0/size;
      E=1e-6;
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      ObjectRef inputValue = getInput(inputID, count);
      ObjectRef refValue = getInput(refID, count);



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
	 can_look_back=true;
	 past = &object_cast<Vector<float> > (pastInputValue);
      }      
      
      DYN_VEC(float, inputLength+size-1, _x);
      //float _x[inputLength+size-1];
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

      float norm;
      for (int i=0;i<inputLength;i++)
      {
	 for (int j=0;j<size;j++)
	    output[i] += a[j]*x[i-j];
	 float err = ref[i]-output[i];
	 
	 E = (1-beta)*E + beta*x[i]*x[i];
	 norm = alpha*err/pow(E, power);
	 for (int j=0;j<size;j++)
	    a[j] += norm*x[i-j];
      }
   }

};

}//namespace FD
