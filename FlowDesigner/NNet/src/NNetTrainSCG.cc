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
#include "ObjectRef.h"
#include "FFNet.h"

class NNetTrainSCG;

DECLARE_NODE(NNetTrainSCG)
/*Node
 *
 * @name NNetTrainSCG
 * @category NNet
 * @description Neural network (MLP) training unsing the scaled conjugate gradient algorithm
 *
 * @input_name TRAIN_IN
 * @input_type Vector
 * @input_description Input data accumulator
 *
 * @input_name TRAIN_OUT
 * @input_type Vector
 * @input_description Output data accumulator
 *
 * @input_name NNET
 * @input_type FFNet
 * @input_description Neural network that will be trained
 *
 * @output_name OUTPUT
 * @output_type FFNet
 * @output_description Trained network
 *
 * @parameter_name MAX_EPOCH
 * @parameter_type int
 * @parameter_description Number of training epoch (default 2000)
 *
 * @parameter_name SIGMA
 * @parameter_type float
 * @parameter_description Sigma parameter
 *
 * @parameter_name LAMBDA
 * @parameter_type float
 * @parameter_description Lambda parameter
 *
END*/


class NNetTrainSCG : public BufferedNode {

protected:
   
   /**The ID of the 'trainIN' input*/
   int trainInID;

   /**The ID of the 'trainOut' input*/
   int trainOutID;

   /**The ID of the 'output' output*/
   int outputID;

   /**The ID of the 'nnet' input*/
   int netInputID;

   int maxEpoch;
      
   float sigma;

   float lambda;

public:
   /**Constructor, takes the name of the node and a set of parameters*/
   NNetTrainSCG(string nodeName, ParameterSet params)
      : BufferedNode(nodeName, params)
   {
      outputID = addOutput("OUTPUT");
      netInputID = addInput("NNET");
      trainInID = addInput("TRAIN_IN");
      trainOutID = addInput("TRAIN_OUT");
      
      if (parameters.exist("MAX_EPOCH"))
	 maxEpoch = dereference_cast<int> (parameters.get("MAX_EPOCH"));
      else maxEpoch = 2000;
      
      if (parameters.exist("SIGMA"))
	 sigma = dereference_cast<float> (parameters.get("SIGMA"));
      else sigma = .01;

      if (parameters.exist("LAMBDA"))
	 lambda = dereference_cast<float> (parameters.get("LAMBDA"));
      else lambda = .0001;
      
   }
      

   /**Ask for the node's output which ID (number) is output_id 
      and for the 'count' iteration */
   virtual void calculate(int output_id, int count, Buffer &out)
   {
      cerr << "getOutput in NNetTrainSCG\n";
      int i,j;

      ObjectRef trainInValue = getInput(trainInID, count);
      ObjectRef trainOutValue = getInput(trainOutID, count);
      ObjectRef netValue = getInput(netInputID, count);
      
      //cerr << "inputs calculated\n";
      Vector<ObjectRef>  &inBuff = object_cast<Vector<ObjectRef> > (trainInValue);
      Vector<ObjectRef>  &outBuff = object_cast<Vector<ObjectRef> > (trainOutValue);
      

      //cerr << "inputs converted\n";
      vector <float *> tin(inBuff.size());
      for (i=0;i<inBuff.size();i++)
	 tin[i]=&object_cast <Vector<float> > (inBuff[i])[0];
      
      vector <float *> tout(outBuff.size());
      for (i=0;i<outBuff.size();i++)
	 tout[i]=&object_cast <Vector<float> > (outBuff[i])[0];
      
      
      FFNet &net = object_cast<FFNet> (netValue);
      //net.setDerivOffset(.05);
      net.trainSCG(tin, tout, maxEpoch, sigma, lambda);
      
      out[count] = netValue;

   }

protected:
   /**Default constructor, should not be used*/
   NNetTrainSCG() {throw new GeneralException("NNetTrainSCG copy constructor should not be called",__FILE__,__LINE__);}

};
