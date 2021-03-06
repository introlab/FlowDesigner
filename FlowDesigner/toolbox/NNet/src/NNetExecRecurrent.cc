// Copyright (C) 1999 Jean-Marc Valin

#include "BufferedNode.h"
#include "Buffer.h"
#include "Vector.h"
#include "FFNet.h"

using namespace std;

namespace FD {

class NNetExecRecurrent;

DECLARE_NODE(NNetExecRecurrent)
/*Node
 *
 * @name NNetExecRecurrent
 * @category NNet
 * @description No description available
 *
 * @input_name INPUT
 * @input_description No description available
 *
 * @input_name NNET
 * @input_description No description available
 *
 * @output_name OUTPUT
 * @output_description No description available
 *
 * @parameter_name OUTPUTLENGTH
 * @parameter_description No description available
 *
END*/


class NNetExecRecurrent : public BufferedNode {
   
   int inputID;
   int netInputID;
   double *prev_output;
   int outputID;
   int outputLength;
public:
   NNetExecRecurrent(string nodeName, ParameterSet params)
   : BufferedNode(nodeName, params)
   {
      inOrder = true;
      inputID = addInput("INPUT");
      netInputID = addInput("NNET");
      outputID = addOutput("OUTPUT");

      outputLength = dereference_cast<int> (parameters.get("OUTPUTLENGTH"));
      prev_output=new double [outputLength];
   }

   ~NNetExecRecurrent() 
   {
      delete [] prev_output;
   }

   virtual void initialize()
   {
      for (int i=0;i<outputLength;i++)
	 prev_output[i]=0;

      this->BufferedNode::initialize();
   }

   virtual void reset()
   {
      for (int i=0;i<outputLength;i++)
	 prev_output[i]=0;

      this->BufferedNode::reset();
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      ObjectRef netValue = getInput(netInputID, count);

      ObjectRef inputValue = getInput(inputID, count);

      const Vector<float> &in = object_cast<Vector<float> > (inputValue);
      int inputLength = in.size();

      Vector<float> &output = *Vector<float>::alloc(outputLength);
      out[count] = &output;


      FFNet &net = object_cast<FFNet> (netValue);
      
      //int classID = vq.getClassID(in.begin());
      //const vector<float> &mean = vq[classID];
      double tmp[in.size()+outputLength];
      for (int i=0;i<in.size();i++)
	 tmp[i]=in[i];
      for (int i=0;i<outputLength;i++)
	 tmp[i+in.size()] = prev_output[i];
      double *netOut = net.calc(tmp);
      for (int i=0;i<outputLength;i++)
	 prev_output[i]=netOut[i];
      
      for (int i=0;i<outputLength;i++)
         output[i]=netOut[i];
       
   }

};
}//namespace FD
