// Copyright (C) 1999 Jean-Marc Valin

#include "GMMTrain.h"
#include "net_types.h"
#include "covariance.h"
#include "gmm.h"
#include "Vector.h"

DECLARE_NODE(GMMTrain)
/*Node
 *
 * @name GMMTrain
 * @category HMM
 * @description Trains a GMM using an accumulator of frames
 *
 * @input_name FRAMES
 * @input_type Vector
 * @input_description Frame accumulator
 *
 * @output_name OUTPUT
 * @output_type GMM
 * @output_description The trained GMM
 *
 * @parameter_name SPLIT_LEVELS
 * @parameter_type int
 * @parameter_description Number of times to perform the split = log2 (number of gaussians)
 * 
END*/


GMMTrain::GMMTrain(string nodeName, ParameterSet params) 
   : Node(nodeName, params)
{
   //cerr << "GMMTrain initialize\n";
   outputID = addOutput("OUTPUT");
   framesInputID = addInput("FRAMES");
   splitLevels = dereference_cast<int> (parameters.get("SPLIT_LEVELS"));
}

void GMMTrain::specificInitialize()
{
   processCount = -1;
   this->Node::specificInitialize();
}

void GMMTrain::reset()
{
   processCount = -1;
   this->Node::reset();
}

ObjectRef GMMTrain::getOutput(int output_id, int count)
{
   //cerr << "Getting output in GMMTrain\n";
   if (output_id==outputID)
   {
      if (count != processCount)
      {
         int i;
         NodeInput framesInput = inputs[framesInputID];

         ObjectRef matRef = framesInput.node->getOutput(framesInput.outputID,count);

         Vector<ObjectRef>  &mat = object_cast<Vector<ObjectRef> > (matRef);

         GMM *gmm = new GMM(1, object_cast <Vector<float> > (mat[0]).size(), NewDiagonalCovariance);
         //cerr << "mat.size(): " << mat.getCurrentPos() << endl;
         vector <float *> data(mat.size());
         for (i=0;i<mat.size();i++)
            data[i]=&object_cast <Vector<float> > (mat[i])[0];
         gmm->init(data);
         gmm->to_real();
         for (i=0;i<splitLevels;i++)
         {
            //gmm->split1();
            gmm->binary_split();
            gmm->kmeans1(data,10);
            cerr << "*******  " << i << "  *******"<<endl;
         }
         //cerr << endl;
         gmm->kmeans1(data,10);

         currentGMM = ObjectRef(gmm);
      }
      return currentGMM;
   }
   else 
      throw new NodeException (this, "GMMTrain: Unknown output id", __FILE__, __LINE__);
}
